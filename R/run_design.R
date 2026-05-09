#' Execute a design end-to-end
#'
#' Runs each step in order, threading the data frame through DGP steps and
#' collecting inquiry and estimator output.
#'
#' @param design A `design`.
#' @return A list with elements `data`, `inquiries`, and `estimates`.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y))
#' res <- run_design(design)
#' nrow(res$data)
run_design <- function(design) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  data <- NULL
  inquiries <- list()
  estimates <- list()
  for (step in design) {
    ct <- attr(step, "causal_type")
    if (is.null(ct)) next
    if (identical(ct, "dgp")) {
      data <- step(data)
    } else if (identical(ct, "inquiry")) {
      inquiries[[length(inquiries) + 1L]] <- step(data)
    } else if (identical(ct, "estimator")) {
      estimates[[length(estimates) + 1L]] <- step(data)
    }
  }
  list(
    data      = data,
    inquiries = dplyr::bind_rows(inquiries),
    estimates = dplyr::bind_rows(estimates)
  )
}

#' Draw the realized data from a design
#'
#' @param design A `design`.
#' @return A data frame.
#' @export
#' @examples
#' design <- declare_model(N = 25, X = rnorm(N))
#' df <- draw_data(design)
#' nrow(df)
draw_data <- function(design) {
  run_design(design)$data
}

#' Draw the realized estimands
#'
#' @param design A `design`.
#' @return A tibble of inquiries (one row per estimand).
#' @export
#' @examples
#' design <- declare_model(N = 25, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y))
#' draw_estimands(design)
draw_estimands <- function(design) {
  run_design(design)$inquiries
}

#' @rdname draw_estimands
#' @export
draw_estimand <- draw_estimands

#' Draw the realized estimates
#'
#' Runs the design once and returns its estimates joined to inquiries.
#'
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
  result <- run_design(design)
  merge_estimates_inquiries(result$estimates, result$inquiries)
}

#' Get estimates for a fixed dataset
#'
#' Runs only the estimator steps of a design against a supplied data frame.
#'
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
  estimates <- list()
  for (step in steps) {
    if (identical(attr(step, "causal_type"), "estimator")) {
      estimates[[length(estimates) + 1L]] <- step(data)
    }
  }
  dplyr::bind_rows(estimates)
}
