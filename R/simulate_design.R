#' Simulate one or more designs
#'
#' Runs each design `sims` times, collecting estimands and estimates into a
#' single long tibble suitable for diagnosis. When more than one design is
#' supplied, a `design` column distinguishes them.
#'
#' If any step in a design has `draws > 1` and `sims` is not supplied, the
#' simulation runs in nested mode: each step with `draws > 1` fans out, and
#' the total simulation count equals the product of all `draws` values across
#' steps. When `sims` is supplied alongside step-level `draws`, the flat
#' `sims` simulation is used and a warning lists the ignored draw values.
#'
#' Parallelism is handled transparently via the `future` ecosystem. Call
#' `future::plan(multisession, workers = 4)` before `simulate_design()` and,
#' if the `furrr` package is installed, simulations will run in parallel with
#' no other changes required.
#'
#' @param ... One or more `design` objects.
#' @param sims Number of simulations per design. Defaults to `NULL`. When
#'   `NULL` and the design has step-level `draws`, the design runs in nested
#'   mode; when `NULL` and the design has no step-level draws, defaults to
#'   `500`.
#' @return A tibble of stacked simulation results.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' simulate_design(design, sims = 3)
simulate_design <- function(..., sims = NULL) {
  raw <- rlang::dots_list(..., .named = TRUE)
  designs <- flatten_designs(raw)
  if (length(designs) == 0) {
    stop("simulate_design() requires at least one `design` object.")
  }
  multi <- length(designs) > 1L
  per_design <- purrr::imap(designs, function(design, design_label) {
    if (design_has_nested_draws(design)) {
      if (!is.null(sims)) warn_sims_draws_conflict(design, sims)
      simulate_nested_single(design, design_label = design_label,
                             multi = multi)
    } else {
      one_design_sims(design, sims = sims %||% 500L,
                      design_label = design_label, multi = multi)
    }
  })
  out <- dplyr::bind_rows(per_design)
  param_names <- unique(unlist(lapply(designs, function(d) {
    p <- attr(d, "parameters")
    if (is.null(p)) character(0) else names(p)
  })))
  if (length(param_names) > 0) {
    attr(out, "parameter_names") <- param_names
  }
  draw_cols <- unique(unlist(lapply(per_design, function(d) {
    attr(d, "draw_cols")
  })))
  if (length(draw_cols) > 0) {
    attr(out, "draw_cols") <- draw_cols
    attr(out, "nested") <- TRUE
  }
  out
}

#' Detect whether any step in a design has step-level draws > 1
#'
#' @keywords internal
#' @noRd
design_has_nested_draws <- function(design) {
  steps <- if (inherits(design, "design")) unclass(design) else list(design)
  any(vapply(steps, function(s) (attr(s, "draws") %||% 1L) > 1L, logical(1)))
}

#' Get the per-step draws vector
#'
#' @keywords internal
#' @noRd
get_step_draws <- function(design) {
  steps <- unclass(design)
  setNames(
    vapply(steps, function(s) as.integer(attr(s, "draws") %||% 1L),
           integer(1)),
    names(steps)
  )
}

#' Warn the user when both `sims` and step-level `draws` are set
#'
#' @keywords internal
#' @noRd
warn_sims_draws_conflict <- function(design, sims) {
  step_draws <- get_step_draws(design)
  total_nested <- prod(step_draws)
  lines <- purrr::imap_chr(step_draws, function(d, nm) {
    flag <- if (d > 1L) paste0("draws = ", d) else "draws = 1"
    sprintf("    %-20s %s", nm, flag)
  })
  rlang::warn(paste0(
    "`sims = ", sims, "` ignored: step-level `draws` are declared and take priority.\n",
    "  Simulation plan:\n",
    paste(lines, collapse = "\n"), "\n",
    "  Running: ", total_nested, " nested paths ",
    "(", paste(step_draws[step_draws > 1L], collapse = " x "), ")."
  ))
}

#' Flatten a list of designs / lists-of-designs / steps
#'
#' Accepts the raw `...` list passed by users to `simulate_design()` and
#' `diagnose_design()`. Promotes bare design steps to single-step designs,
#' recursively flattens lists (so the output of `redesign()` is accepted
#' transparently), and preserves user-supplied names where available.
#'
#' @param raw A list of designs, design_steps, or lists thereof.
#' @return A flat named list of `design` objects.
#' @keywords internal
#' @noRd
flatten_designs <- function(raw) {
  out <- list()
  for (i in seq_along(raw)) {
    item <- raw[[i]]
    nm <- names(raw)[i]
    if (inherits(item, "design")) {
      label <- if (!is.null(nm) && nzchar(nm)) nm else
        paste0("design_", length(out) + 1L)
      out[[label]] <- item
    } else if (inherits(item, "design_step")) {
      label <- if (!is.null(nm) && nzchar(nm)) nm else
        paste0("design_", length(out) + 1L)
      out[[label]] <- construct_design(wrap_step(item))
    } else if (is.list(item)) {
      sub <- flatten_designs(item)
      if (length(sub) > 0) {
        sub_names <- names(sub)
        if (!is.null(nm) && nzchar(nm) && length(sub) == 1L) {
          names(sub) <- nm
        } else if (!is.null(nm) && nzchar(nm)) {
          names(sub) <- paste0(nm, "_", sub_names)
        }
        for (k in names(sub)) {
          label <- k
          if (label %in% names(out)) {
            label <- make.unique(c(names(out), label))[length(out) + 1L]
          }
          out[[label]] <- sub[[k]]
        }
      }
    }
  }
  out
}

#' @rdname simulate_design
#' @export
simulate_designs <- simulate_design

#' Simulate a single design (flat)
#'
#' @keywords internal
#' @noRd
one_design_sims <- function(design, sims, design_label = "design",
                            multi = FALSE) {
  map_fn <- sim_map_fn()
  results <- map_fn(seq_len(sims), function(i) {
    r <- run_design(design)
    list(inquiries = r$inquiries, estimates = r$estimates)
  })
  inquiries_df <- dplyr::bind_rows(
    purrr::map(results, "inquiries"),
    .id = "sim_ID"
  )
  estimates_df <- dplyr::bind_rows(
    purrr::map(results, "estimates"),
    .id = "sim_ID"
  )
  if (nrow(inquiries_df) > 0) {
    inquiries_df$sim_ID <- as.integer(inquiries_df$sim_ID)
  }
  if (nrow(estimates_df) > 0) {
    estimates_df$sim_ID <- as.integer(estimates_df$sim_ID)
  }
  out <- merge_estimates_inquiries(estimates_df, inquiries_df)
  if (multi && nrow(out) > 0) {
    out$design <- design_label
    out <- dplyr::relocate(out, "design")
  }
  params <- attr(design, "parameters")
  if (!is.null(params) && nrow(out) > 0) {
    for (nm in names(params)) {
      if (!nm %in% names(out)) out[[nm]] <- params[[nm]]
    }
  }
  out
}

#' Simulate a single design with nested step-level draws
#'
#' Each step with `draws > 1` fans out: subsequent steps are re-executed for
#' every draw of that step, recursively. The outermost fan-out level (when it
#' is a DGP step) is parallelised via `sim_map_fn()`; inner draws run
#' sequentially within each outer iteration.
#'
#' @param design A single `design` object.
#' @param design_label Name of the design (only used when `multi = TRUE`).
#' @param multi Whether multiple designs are being simulated.
#' @return A tibble of merged estimates and inquiries with one
#'   `<step_label>_draw` column per fan-out step.
#' @keywords internal
#' @noRd
simulate_nested_single <- function(design, design_label = "design",
                                   multi = FALSE) {
  steps <- unclass(design)
  step_draws <- vapply(steps, function(s) as.integer(attr(s, "draws") %||% 1L),
                       integer(1))
  step_types <- vapply(steps, function(s) attr(s, "causal_type") %||% NA_character_,
                       character(1))
  step_labels <- names(steps)
  fan_steps <- which(step_draws > 1L)

  # run_from: execute steps[step_idx..end] against `data`, fanning out at any
  # step with draws > 1. Returns a list with inquiries, estimates collected
  # across all draws below this point.
  run_from <- function(step_idx, data, draw_cols = list()) {
    if (step_idx > length(steps)) {
      return(list(
        inquiries = tibble::tibble(),
        estimates = tibble::tibble()
      ))
    }
    step  <- steps[[step_idx]]
    n     <- step_draws[[step_idx]]
    ct    <- step_types[[step_idx]]
    label <- step_labels[[step_idx]]

    draw_results <- lapply(seq_len(n), function(d) {
      new_draw_cols <- draw_cols
      if (n > 1L) new_draw_cols[[paste0(label, "_draw")]] <- d
      if (identical(ct, "dgp")) {
        new_data <- step(data)
        run_from(step_idx + 1L, new_data, new_draw_cols)
      } else if (identical(ct, "inquiry")) {
        inq <- step(data)
        sub <- run_from(step_idx + 1L, data, new_draw_cols)
        if (nrow(inq) > 0) {
          for (nm in names(new_draw_cols)) inq[[nm]] <- new_draw_cols[[nm]]
        }
        list(
          inquiries = dplyr::bind_rows(inq, sub$inquiries),
          estimates = sub$estimates
        )
      } else if (identical(ct, "estimator")) {
        est <- step(data)
        sub <- run_from(step_idx + 1L, data, new_draw_cols)
        if (nrow(est) > 0) {
          for (nm in names(new_draw_cols)) est[[nm]] <- new_draw_cols[[nm]]
        }
        list(
          inquiries = sub$inquiries,
          estimates = dplyr::bind_rows(est, sub$estimates)
        )
      } else {
        run_from(step_idx + 1L, data, new_draw_cols)
      }
    })
    list(
      inquiries = dplyr::bind_rows(purrr::map(draw_results, "inquiries")),
      estimates = dplyr::bind_rows(purrr::map(draw_results, "estimates"))
    )
  }

  map_fn <- sim_map_fn()
  first_fan <- if (length(fan_steps) > 0) fan_steps[1] else NA_integer_

  if (!is.na(first_fan) && identical(step_types[[first_fan]], "dgp")) {
    n_outer <- step_draws[[first_fan]]
    outer_results <- map_fn(seq_len(n_outer), function(outer_d) {
      data <- NULL
      # Run all DGP steps up to and including the first fan step.
      # Inquiry / estimator steps before that point would be premature, but in
      # principle there should not be any since the first fan-out is the first
      # step touched. Inquiries / estimators ahead of the first fan step
      # belong outside the fan and are evaluated in the inner recursion under
      # data == NULL, which is meaningless. The convention is that the first
      # fan-out step is a DGP step (model / sampling / assignment).
      for (i in seq_len(first_fan)) {
        if (identical(step_types[[i]], "dgp")) {
          data <- steps[[i]](data)
        }
      }
      dc <- list()
      dc[[paste0(step_labels[[first_fan]], "_draw")]] <- outer_d
      run_from(first_fan + 1L, data, dc)
    })
  } else {
    outer_results <- list(run_from(1L, NULL, list()))
  }

  inquiries_df <- dplyr::bind_rows(purrr::map(outer_results, "inquiries"),
                                   .id = "sim_ID")
  estimates_df <- dplyr::bind_rows(purrr::map(outer_results, "estimates"),
                                   .id = "sim_ID")
  if (nrow(inquiries_df) > 0) {
    inquiries_df$sim_ID <- as.integer(inquiries_df$sim_ID)
  }
  if (nrow(estimates_df) > 0) {
    estimates_df$sim_ID <- as.integer(estimates_df$sim_ID)
  }

  # Override sim_ID to be a globally unique simulation identifier.
  # In nested mode, sim_ID identifies a unique combination of all draw_cols.
  draw_col_names <- paste0(step_labels[step_draws > 1L], "_draw")
  inquiries_draw_cols <- intersect(draw_col_names, names(inquiries_df))
  estimates_draw_cols <- intersect(draw_col_names, names(estimates_df))

  if (length(estimates_draw_cols) > 0 && nrow(estimates_df) > 0) {
    key <- do.call(paste, c(
      lapply(estimates_draw_cols, function(c) estimates_df[[c]]),
      list(sep = "\r")
    ))
    estimates_df$sim_ID <- as.integer(factor(key, levels = unique(key)))
  }
  if (length(inquiries_draw_cols) > 0 && nrow(inquiries_df) > 0) {
    key <- do.call(paste, c(
      lapply(inquiries_draw_cols, function(c) inquiries_df[[c]]),
      list(sep = "\r")
    ))
    inquiries_df$sim_ID <- as.integer(factor(key, levels = unique(key)))
  }

  # For the merge to align estimates with inquiries from the same world,
  # the join keys must include the draw columns for steps that come BEFORE
  # the inquiry step (i.e., DGP-type fan-outs upstream of the inquiry).
  # The simplest strategy: include any draw column present on the inquiries
  # table in the merge.
  out <- merge_estimates_inquiries_nested(estimates_df, inquiries_df,
                                          inquiries_draw_cols)

  if (multi && nrow(out) > 0) {
    out$design <- design_label
    out <- dplyr::relocate(out, "design")
  }
  params <- attr(design, "parameters")
  if (!is.null(params) && nrow(out) > 0) {
    for (nm in names(params)) {
      if (!nm %in% names(out)) out[[nm]] <- params[[nm]]
    }
  }
  attr(out, "draw_cols") <- intersect(draw_col_names, names(out))
  attr(out, "nested") <- TRUE
  out
}

#' Merge estimates with inquiries in the nested-simulation case
#'
#' Like [merge_estimates_inquiries()] but additionally joins on any
#' upstream-fanout draw columns that appear on the inquiries table, so
#' estimators are linked to the right realised estimand.
#'
#' @keywords internal
#' @noRd
merge_estimates_inquiries_nested <- function(estimates, inquiries,
                                             inquiry_draw_cols) {
  if (nrow(estimates) == 0 && nrow(inquiries) == 0) return(tibble::tibble())
  if (nrow(estimates) == 0) return(tibble::as_tibble(inquiries))
  if (nrow(inquiries) == 0) return(tibble::as_tibble(estimates))
  if (!"inquiry" %in% names(estimates)) {
    return(tibble::as_tibble(estimates))
  }
  base_keys <- intersect(c("design", "inquiry"), names(estimates))
  base_keys <- intersect(base_keys, names(inquiries))
  draw_keys <- intersect(inquiry_draw_cols, names(estimates))
  draw_keys <- intersect(draw_keys, names(inquiries))
  join_cols <- c(base_keys, draw_keys)
  if (length(join_cols) == 0) return(tibble::as_tibble(estimates))
  result <- dplyr::left_join(
    tibble::as_tibble(estimates),
    tibble::as_tibble(inquiries),
    by = join_cols,
    suffix = c("", ".inquiry"),
    relationship = "many-to-many"
  )
  result
}
