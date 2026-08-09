#' Simulate one or more designs
#'
#' Runs each design `sims` times, collecting estimands and estimates into a
#' single long tibble suitable for diagnosis. When more than one design is
#' supplied, a `design` column distinguishes them.
#'
#' If any step in a design has `draws > 1`, the simulation runs in nested
#' mode: each step with `draws > 1` fans out, and the total simulation count
#' equals the product of all `draws` values across steps. Declared `draws`
#' take priority over `sims`, so supplying both runs the nested simulation and
#' warns, printing the plan it is running instead.
#'
#' Steps upstream of the first fan-out run once and are held fixed across the
#' draws below them, which is what makes `declare_assignment(..., draws = 50)`
#' mean fifty randomizations of one population.
#'
#' Parallelism is handled transparently via the `future` ecosystem. Call
#' `future::plan(multisession, workers = 4)` before `simulate_design()` and,
#' if the `furrr` package is installed, simulations will run in parallel with
#' no other changes required.
#'
#' @family simulation and diagnosis
#' @param ... One or more `design` objects.
#' @param sims Number of simulations per design. Defaults to `NULL`, which
#'   means 500 flat simulations for a design with no step-level `draws`. A
#'   design with step-level `draws` runs in nested mode whether or not `sims`
#'   is supplied; supplying it warns and is otherwise ignored.
#' @param progress If `TRUE`, display a progress bar for this call by wrapping
#'   it in [progressr::with_progress()]. The better habit is to opt in once per
#'   session with `progressr::handlers(global = TRUE)`, which covers every call
#'   and lets you choose how progress is shown. Nothing is displayed by default.
#' @return A tibble of stacked simulation results.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' simulate_design(design, sims = 3)
simulate_design <- function(..., sims = NULL, progress = FALSE) {
  if (isTRUE(progress)) {
    return(with_dd_progress(simulate_design(..., sims = sims, progress = FALSE)))
  }
  raw <- name_design_dots(...)
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
  attr(out, "matched_on") <- unique(unlist(lapply(per_design, function(d) {
    attr(d, "matched_on")
  })))
  out
}

#' Write a design's redesign parameters onto its simulations table
#'
#' A parameter that is not an atomic scalar (a function, say) is written as
#' its deparsed source, so the column stays a plain vector that diagnosis can
#' group by and print.
#'
#' @keywords internal
#' @noRd
attach_parameters <- function(out, params) {
  if (is.null(params) || nrow(out) == 0) return(out)
  for (nm in names(params)) {
    if (nm %in% names(out)) next
    value <- params[[nm]]
    if (is.list(value)) value <- value[[1]]
    if (!(is.atomic(value) && length(value) == 1L)) {
      value <- paste(deparse(value), collapse = " ")
    }
    out[[nm]] <- value
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

#' Label the designs passed through `...`
#'
#' A dot takes its expression as a label only when that expression is a bare
#' symbol *and* it holds a single design, so `simulate_design(design_a,
#' design_b)` labels its two designs. A list of designs keeps its own names,
#' whether it is written out (`list(dum = d, dee = d)`) or reached through a
#' symbol, rather than having the enclosing expression pasted onto them. Names
#' the caller supplied always win.
#'
#' @param ... Designs, lists of designs, or design steps.
#' @return The dots as a list, named where a label could be found.
#' @keywords internal
#' @noRd
name_design_dots <- function(...) {
  items <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  nms <- rlang::names2(items)
  single <- vapply(items, inherits, logical(1),
                   what = c("design", "design_step"), which = FALSE)
  auto <- !nzchar(nms) & single &
    vapply(exprs, rlang::is_symbol, logical(1))
  nms[auto] <- vapply(exprs[auto], rlang::as_string, character(1))
  rlang::set_names(items, nms)
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
  map_fn <- sim_map_fn(paste0("Simulating ", design_label))
  results <- map_fn(seq_len(sims), function(i) {
    r <- run_design_internal(design)
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
  warn_estimator_failures(estimates_df, design_label)
  if (nrow(inquiries_df) > 0) {
    inquiries_df$sim_ID <- as.integer(inquiries_df$sim_ID)
  }
  if (nrow(estimates_df) > 0) {
    estimates_df$sim_ID <- as.integer(estimates_df$sim_ID)
  }
  out <- merge_estimates_inquiries(estimates_df, inquiries_df)
  matched_on <- attr(out, "matched_on")
  if (multi && nrow(out) > 0) {
    out$design <- design_label
    out <- dplyr::relocate(out, "design")
  }
  out <- attach_parameters(out, attr(design, "parameters"))
  attr(out, "matched_on") <- matched_on
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
  # across all draws below this point. `pin` fixes this step to a single draw
  # index, which is how the outermost fan-out is distributed across workers.
  run_from <- function(step_idx, data, draw_cols = list(), pin = NULL) {
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

    draw_ids <- if (is.null(pin)) seq_len(n) else pin
    draw_results <- lapply(draw_ids, function(d) {
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

  # Steps upstream of the first fan-out sit outside the fan and run exactly
  # once. Re-running them per draw would redraw the very thing the fan is meant
  # to hold fixed: `declare_assignment(..., draws = 50)` asks for 50
  # randomizations of one population, not 50 populations.
  run_prefix <- function(idx) {
    data <- NULL
    inquiries <- list()
    estimates <- list()
    for (i in idx) {
      ct <- step_types[[i]]
      if (identical(ct, "dgp")) {
        data <- steps[[i]](data)
      } else if (identical(ct, "inquiry")) {
        inquiries[[length(inquiries) + 1L]] <- steps[[i]](data)
      } else if (identical(ct, "estimator")) {
        estimates[[length(estimates) + 1L]] <- steps[[i]](data)
      }
    }
    list(data      = data,
         inquiries = dplyr::bind_rows(inquiries),
         estimates = dplyr::bind_rows(estimates))
  }

  map_fn <- sim_map_fn("Simulating")
  first_fan <- if (length(fan_steps) > 0) fan_steps[1] else NA_integer_

  if (!is.na(first_fan)) {
    prefix  <- run_prefix(seq_len(first_fan - 1L))
    n_outer <- step_draws[[first_fan]]
    outer_results <- map_fn(seq_len(n_outer), function(outer_d) {
      run_from(first_fan, prefix$data, list(), pin = outer_d)
    })
    if (nrow(prefix$inquiries) > 0 || nrow(prefix$estimates) > 0) {
      outer_results <- c(
        list(list(inquiries = prefix$inquiries, estimates = prefix$estimates)),
        outer_results
      )
    }
  } else {
    outer_results <- list(run_from(1L, NULL, list()))
  }

  inquiries_df <- dplyr::bind_rows(purrr::map(outer_results, "inquiries"),
                                   .id = "sim_ID")
  estimates_df <- dplyr::bind_rows(purrr::map(outer_results, "estimates"),
                                   .id = "sim_ID")
  warn_estimator_failures(estimates_df)
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
  matched_on <- attr(out, "matched_on")

  if (multi && nrow(out) > 0) {
    out$design <- design_label
    out <- dplyr::relocate(out, "design")
  }
  out <- attach_parameters(out, attr(design, "parameters"))
  attr(out, "matched_on") <- matched_on
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
  base_keys <- intersect(c("design", "inquiry"), names(estimates))
  base_keys <- intersect(base_keys, names(inquiries))
  draw_keys <- intersect(inquiry_draw_cols, names(estimates))
  draw_keys <- intersect(draw_keys, names(inquiries))
  join_cols <- c(base_keys, draw_keys)
  result <- if (length(join_cols) == 0) {
    dplyr::cross_join(tibble::as_tibble(estimates),
                      tibble::as_tibble(inquiries),
                      suffix = c("", ".inquiry"))
  } else {
    dplyr::left_join(
      tibble::as_tibble(estimates),
      tibble::as_tibble(inquiries),
      by = join_cols,
      suffix = c("", ".inquiry"),
      relationship = "many-to-many"
    )
  }
  attr(result, "matched_on") <- join_cols
  result
}
