#' Execute a design end-to-end
#'
#' Runs the design once and returns one row per estimate, with the realized
#' estimands joined on. This is one simulation: `simulate_design(sims = 1)`
#' without the `sim_ID` column.
#'
#' Use [draw_data()] for the realized data frame, [draw_estimands()] for the
#' inquiries alone, and [draw_estimates()] for the estimates alone.
#'
#' @family drawing from a design
#' @param design A `design`.
#' @return A tibble of estimates with estimands joined where applicable.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y))
#' run_design(design)
run_design <- function(design) {
  result <- run_design_internal(design)
  stop_on_estimator_failure(result$estimates)
  merge_estimates_inquiries(result$estimates, result$inquiries)
}

#' Execute a design and keep the data alongside the results
#'
#' The workhorse behind [run_design()] and the `draw_*()` family. Returns the
#' three pieces separately, before estimates and inquiries are joined.
#'
#' @param design A `design`.
#' @param what Which kinds of step to run. `draw_data()` asks for `"dgp"`
#'   alone and `draw_estimands()` for the inquiries, so a failing estimator
#'   does not take them down with it and neither pays to fit models it will
#'   discard.
#' @return A list with elements `data`, `inquiries`, and `estimates`.
#' @keywords internal
#' @noRd
run_design_internal <- function(design,
                                what = c("dgp", "inquiry", "estimator")) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  data <- NULL
  inquiries <- list()
  estimates <- list()
  # A note is taken during the run, so the steps after it are rebound on every
  # draw against a local copy of the step list. The design itself is never
  # touched, which is what keeps one draw's notes out of the next one.
  steps <- unclass(design)
  notes <- list()
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    if (is_notes_step(step)) {
      notes <- record_notes(notes, note_values(step, data, notes))
      steps <- apply_notes_from(steps, i, notes)
      next
    }
    ct <- attr(step, "causal_type")
    if (is.null(ct) || !ct %in% what) next
    if (identical(ct, "dgp")) {
      data <- run_step(step, data)
    } else if (identical(ct, "inquiry")) {
      inquiries[[length(inquiries) + 1L]] <- run_step(step, data)
    } else if (identical(ct, "estimator")) {
      estimates[[length(estimates) + 1L]] <- run_step(step, data)
    }
  }
  list(
    data      = data,
    inquiries = dplyr::bind_rows(inquiries),
    estimates = dplyr::bind_rows(estimates)
  )
}

#' Run one step, naming it if it fails
#'
#' A column that does not exist fails inside `eval_tidy()` with "object 'W'
#' not found" and nothing else, and a five-step design has five places that
#' could have been. The step's label and verb go in front; the original
#' condition is kept as the parent, so anything catching by class still can.
#'
#' @keywords internal
#' @noRd
run_step <- function(step, data) {
  tryCatch(
    step(data),
    error = function(e) {
      rlang::abort(
        paste0("In step `", attr(step, "label") %||% "?", "` (",
               step_verb(step), "())."),
        parent = e, call = NULL
      )
    }
  )
}

#' Draw the realized data from a design
#'
#' Runs the data-generating steps only. The inquiries and estimators are not
#' run, so a design whose estimator fails still draws its data, and drawing
#' data does not pay to fit models it would throw away.
#'
#' @family drawing from a design
#' @param design A `design`.
#' @return A data frame.
#' @export
#' @examples
#' design <- declare_model(N = 25, X = rnorm(N))
#' df <- draw_data(design)
#' nrow(df)
draw_data <- function(design) {
  run_design_internal(design, what = "dgp")$data
}

#' Draw the realized estimands
#'
#' Runs the data-generating and inquiry steps only, not the estimators.
#'
#' @family drawing from a design
#' @param design A `design`.
#' @return A tibble of inquiries (one row per estimand).
#' @export
#' @examples
#' design <- declare_model(N = 25, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y))
#' draw_estimands(design)
draw_estimands <- function(design) {
  run_design_internal(design, what = c("dgp", "inquiry"))$inquiries
}

#' @rdname draw_estimands
#' @export
draw_estimand <- draw_estimands

#' Draw the realized estimates
#'
#' Runs the design once and returns its estimates joined to inquiries.
#'
#' @family drawing from a design
#' @param design A `design`.
#' @return A tibble of estimates with estimands joined where applicable.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y)) +
#'   declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
#'                     inquiry = "mu", label = "ols")
#' draw_estimates(design)
draw_estimates <- function(design) {
  result <- run_design_internal(design)
  stop_on_estimator_failure(result$estimates)
  merge_estimates_inquiries(result$estimates, result$inquiries)
}

#' Get estimates for a fixed dataset
#'
#' Runs only the estimator steps of a design against a supplied data frame.
#'
#' @family drawing from a design
#' @param design A `design`.
#' @param data A data frame.
#' @param start Integer; first step index to consider.
#' @param end Integer; last step index to consider.
#' @return A tibble of estimates.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
#' df <- draw_data(design)
#' get_estimates(design, df)
get_estimates <- function(design, data = draw_data(design), start = 1L,
                          end = length(design)) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  steps <- unclass(design)[seq.int(start, end)]
  # Notes are taken against the supplied data rather than against the data
  # they would have seen in a full run, which is the only thing available
  # here. A note that has to be computed before a sampling step therefore
  # reads the sample, so a design that depends on one is better run than
  # re-estimated.
  notes <- list()
  estimates <- list()
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    if (is_notes_step(step)) {
      notes <- record_notes(notes, note_values(step, data, notes))
      steps <- apply_notes_from(steps, i, notes)
      next
    }
    if (identical(attr(step, "causal_type"), "estimator")) {
      estimates[[length(estimates) + 1L]] <- run_step(step, data)
    }
  }
  dplyr::bind_rows(estimates)
}
