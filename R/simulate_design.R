#' Simulate one or more designs
#'
#' Runs each design `sims` times, collecting estimands and estimates into a
#' single long tibble suitable for diagnosis. When more than one design is
#' supplied, a `design` column distinguishes them.
#'
#' @param ... One or more `design` objects.
#' @param sims Number of simulations per design.
#' @return A tibble of stacked simulation results.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' simulate_design(design, sims = 3)
simulate_design <- function(..., sims = 500) {
  raw <- rlang::dots_list(..., .named = TRUE)
  designs <- flatten_designs(raw)
  if (length(designs) == 0) {
    stop("simulate_design() requires at least one `design` object.")
  }
  multi <- length(designs) > 1L
  per_design <- purrr::imap(designs, function(design, design_label) {
    one_design_sims(design, sims = sims, design_label = design_label,
                    multi = multi)
  })
  out <- dplyr::bind_rows(per_design)
  param_names <- unique(unlist(lapply(designs, function(d) {
    p <- attr(d, "parameters")
    if (is.null(p)) character(0) else names(p)
  })))
  if (length(param_names) > 0) {
    attr(out, "parameter_names") <- param_names
  }
  out
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

#' Simulate a single design
#'
#' @keywords internal
#' @noRd
one_design_sims <- function(design, sims, design_label = "design",
                            multi = FALSE) {
  results <- purrr::map(seq_len(sims), function(i) {
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
