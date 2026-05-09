#' Declare diagnosands
#'
#' Defines the summary statistics computed across simulations during
#' diagnosis. Each named expression is evaluated against the simulations
#' data frame, grouped by `inquiry`, `estimator`, `term`, and `outcome`
#' where present.
#'
#' @param ... Named expressions defining diagnosands.
#' @param label Step label.
#' @return A `design_step` whose `dots` attribute carries the diagnosand
#'   quosures.
#' @export
#' @examples
#' diags <- declare_diagnosands(
#'   bias = mean(estimate - estimand, na.rm = TRUE),
#'   rmse = sqrt(mean((estimate - estimand)^2, na.rm = TRUE))
#' )
#' names(attr(diags, "dots"))
declare_diagnosands <- function(..., label = "diagnosands") {
  dots <- rlang::enquos(...)
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
    call        = call
  )
}

#' Default diagnosands
#'
#' Returns the standard set of diagnosands: mean estimand, mean estimate,
#' bias, SD of estimates, RMSE, power, and coverage of nominal 95 percent
#' confidence intervals.
#'
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
