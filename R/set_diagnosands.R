#' Set the diagnosands for a design
#'
#' A researcher often has a set of diagnosands in mind to appropriately assess the quality of a design. \code{set_diagnosands} sets the default diagnosands for a design, so that later readers can assess the design on the same terms as the original author. Readers can also use \code{diagnose_design} to diagnose the design using any other set of diagnosands.
#'
#' @param design A design typically created using the + operator
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}
#'
#' @return a design object with a diagnosand attribute
#'
#' @examples
#'
#' design <-
#' declare_population(data = sleep) +
#'   declare_estimand(mean_outcome = mean(extra)) +
#'   declare_sampling(n = 10) +
#'   declare_estimator(extra ~ 1, estimand = "mean_outcome",
#'      term = '(Intercept)', model = lm_robust)
#'
#' diagnosands <- declare_diagnosands(
#'   median_bias = median(estimate - estimand), keep_defaults = FALSE)
#'
#' design <- set_diagnosands(design, diagnosands)
#'
#' \dontrun{
#' diagnose_design(design)
#' }
#'
#' @export
set_diagnosands <- function(design, diagnosands = default_diagnosands) {
  attr(design, "diagnosands") <- diagnosands

  design
}
