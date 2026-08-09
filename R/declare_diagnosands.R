#' Declare diagnosands
#'
#' Defines the summary statistics computed across simulations during
#' diagnosis. Each named expression is evaluated against the simulations
#' data frame, grouped by `inquiry`, `estimator`, `term`, and `outcome`
#' where present.
#'
#' @family diagnosands
#' @param ... Named expressions defining diagnosands.
#' @param subset An expression evaluated on the simulations table; only rows
#'   for which it is `TRUE` enter the diagnosands. `NULL` (the default) keeps
#'   every simulation.
#' @param alpha Significance level. Any diagnosand expression that mentions
#'   `alpha` sees this value.
#' @param label Step label.
#' @return A `design_step` whose `dots` attribute carries the diagnosand
#'   quosures.
#' @export
#' @examples
#' diags <- declare_diagnosands(
#'   bias = mean(estimate - estimand, na.rm = TRUE),
#'   rmse = sqrt(mean((estimate - estimand)^2, na.rm = TRUE))
#' )
#'
#' design <-
#'   declare_model(N = 30, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = sample(rep(0:1, length.out = N))) +
#'   declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")
#'
#' diagnose_design(design, sims = 5, bootstrap_sims = 0, diagnosands = diags)
#'
#' # power at the 10 percent level, computed on significant simulations only
#' declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
declare_diagnosands <- function(..., subset = NULL, alpha = 0.05,
                                label = "diagnosands") {
  dots <- capture_dots_env(bind_alpha(rlang::enquos(...), alpha))
  subset_quo <- capture_quosure_env(unwrap_quosure(rlang::enquo(subset)))
  call <- sys.call()
  fn <- function(data) {
    out <- purrr::imap(dots, function(q, nm) {
      val <- rlang::eval_tidy(q, data = as.list(data))
      tibble::tibble(diagnosand = nm, value = val)
    })
    dplyr::bind_rows(out)
  }
  build_step(
    fn          = fn,
    handler_expr = quote(declare_diagnosands),
    dots        = dots,
    step_type   = "diagnosand",
    causal_type = "diagnosands",
    label       = label,
    call        = call,
    subset_quo  = subset_quo
  )
}

#' Make `alpha` resolve to the declared value inside diagnosand expressions
#'
#' A diagnosand quosure carries the environment it was written in, so a bare
#' `alpha` would otherwise resolve to whatever the caller happens to have
#' bound, not to the `alpha` argument. Any quosure mentioning the symbol gets
#' a cloned environment with the declared value bound in it.
#'
#' @keywords internal
#' @noRd
bind_alpha <- function(dots, alpha) {
  purrr::map(dots, function(q) {
    expr <- rlang::quo_get_expr(q)
    if (!expr_has_symbol(expr, "alpha")) return(q)
    env <- rlang::env_clone(rlang::quo_get_env(q))
    rlang::env_bind(env, alpha = alpha)
    rlang::new_quosure(expr, env = env)
  })
}

#' Unwrap a quosure that was injected into a quosure-capturing argument
#'
#' `select_diagnosands()` forwards its `subset` to [declare_diagnosands()] with
#' `!!`, so the captured expression is itself a quosure. Returns `NULL` for an
#' absent subset.
#'
#' @keywords internal
#' @noRd
unwrap_quosure <- function(quo) {
  inner <- rlang::quo_get_expr(quo)
  if (rlang::is_quosure(inner)) quo <- inner
  if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) return(NULL)
  quo
}

#' Default diagnosands
#'
#' Returns the standard set of diagnosands. A diagnosand is a summary of a
#' design's behaviour across simulations, so each one is computed over the
#' whole set of draws rather than within any single draw.
#'
#' \describe{
#'   \item{`mean_estimand`}{Average of the estimand across draws. The estimand
#'     varies from draw to draw whenever the model is stochastic.}
#'   \item{`mean_estimate`}{Average of the estimator's estimate across draws.}
#'   \item{`bias`}{Average of estimate minus estimand. Zero means the estimator
#'     lands on the estimand on average, not that it is close on any one draw.}
#'   \item{`sd_estimate`}{Standard deviation of the estimates, which is the
#'     true sampling variability of the design.}
#'   \item{`rmse`}{Root mean squared error, the average distance between
#'     estimate and estimand. It counts bias and variability together, so it is
#'     the one to read when an estimator trades one against the other.}
#'   \item{`power`}{Share of draws in which the p-value is at or below 0.05.
#'     Read it as power only when the effect is real; under a true null the
#'     same number is the false positive rate.}
#'   \item{`coverage`}{Share of draws in which the confidence interval contains
#'     the estimand. A 95 percent interval should give 0.95. Materially lower
#'     means the intervals are too narrow and the design overstates its own
#'     precision.}
#' }
#'
#' @family diagnosands
#' @return A diagnosand `design_step`.
#' @export
#' @examples
#' default_diagnosands()
default_diagnosands <- function() {
  declare_diagnosands(
    mean_estimand = mean(estimand, na.rm = TRUE),
    mean_estimate = mean(estimate, na.rm = TRUE),
    bias          = mean(estimate - estimand, na.rm = TRUE),
    sd_estimate   = sd(estimate, na.rm = TRUE),
    rmse          = sqrt(mean((estimate - estimand)^2, na.rm = TRUE)),
    power         = mean(p.value <= 0.05, na.rm = TRUE),
    coverage      = mean(conf.low <= estimand & estimand <= conf.high, na.rm = TRUE)
  )
}
