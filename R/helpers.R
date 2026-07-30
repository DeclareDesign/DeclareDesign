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

# Pick the best available map function for the simulation loop.
# Only switches to furrr::future_map when (a) furrr is installed AND (b) the
# active future plan is actually parallel. Under the default sequential plan,
# furrr adds overhead with no benefit. With a parallel plan, furrr::future_map
# is used with seed = TRUE for statistically valid parallel RNG (L'Ecuyer-CMRG).
# Users enable parallelism with future::plan(multisession, workers = N) before
# calling simulate_design() -- no other changes needed.
sim_map_fn <- function() {
  has_furrr <- requireNamespace("furrr", quietly = TRUE)
  has_future <- requireNamespace("future", quietly = TRUE)
  if (has_furrr && has_future &&
      !inherits(future::plan(), "sequential")) {
    # Snapshot attached packages so workers can load the same ones.
    # This ensures functions like complete_ra(), lm_robust(), etc. referenced
    # in quosure environments are available on each worker.
    pkgs <- setdiff(
      sub("^package:", "", grep("^package:", search(), value = TRUE)),
      c("base", ".GlobalEnv", "Autoloads")
    )
    opts <- furrr::furrr_options(seed = TRUE, packages = pkgs)
    function(x, f, ...) furrr::future_map(x, f, ..., .options = opts)
  } else {
    purrr::map
  }
}

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

