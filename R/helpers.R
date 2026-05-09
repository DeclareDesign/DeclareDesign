#' Population variance
#'
#' Computes the (uncorrected) variance: the mean squared deviation from the
#' mean. Useful for design-based standard errors.
#'
#' @param x A numeric vector.
#' @return A scalar.
#' @export
#' @examples
#' pop.var(c(1, 2, 3, 4, 5))
pop.var <- function(x) mean((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)

#' Capture variables as quosures
#'
#' Convenience alias for [rlang::quos()] used inside design declarations to
#' pass through bare expressions.
#'
#' @param ... Expressions.
#' @return A list of quosures.
#' @export
#' @examples
#' qs <- vars(x, y, z)
#' length(qs)
vars <- function(...) rlang::quos(...)

#' Get the simulations table from a diagnosis
#'
#' @param diagnosis A `diagnosis` object.
#' @return A tibble of simulations.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' get_simulations(d)
get_simulations <- function(diagnosis) diagnosis$simulations_df

#' Get the diagnosands table from a diagnosis
#'
#' @param diagnosis A `diagnosis` object.
#' @return A tibble of diagnosands.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' get_diagnosands(d)
get_diagnosands <- function(diagnosis) diagnosis$diagnosands_df

#' Reshape a diagnosis (returns the diagnosands table)
#'
#' @param diagnosis A `diagnosis` object.
#' @param ... Reserved for future use.
#' @return A tibble.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' reshape_diagnosis(d)
reshape_diagnosis <- function(diagnosis, ...) {
  diagnosis$diagnosands_df
}
