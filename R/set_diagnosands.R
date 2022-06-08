#' Set the diagnosands for a design
#'
#' A researcher often has a set of diagnosands in mind to appropriately assess the quality of a design. \code{set_diagnosands} sets the default diagnosands for a design, so that later readers can assess the design on the same terms as the original author. Readers can also use \code{diagnose_design} to diagnose the design using any other set of diagnosands.
#'
#' @param x A design typically created using the + operator, or a simulations data.frame created by \code{simulate_design}.
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}
#'
#' @return a design object with a diagnosand attribute
#'
#' @examples
#'
#' design <-
#' declare_model(data = sleep) +
#'   declare_inquiry(mean_outcome = mean(extra)) +
#'   declare_sampling(S = complete_rs(N, n = 10)) +
#'   declare_estimator(extra ~ 1, inquiry = "mean_outcome",
#'      term = '(Intercept)', .method = lm_robust)
#'
#' diagnosands <- declare_diagnosands(
#'   median_bias = median(estimate - inquiry))
#'
#' design <- set_diagnosands(design, diagnosands)
#'
#' \dontrun{
#' diagnose_design(design)
#' 
#' simulations_df <- simulate_design(design)
#' 
#' simulations_df <- set_diagnosands(simulations_df, design)
#' 
#' diagnose_design(simulations_df)
#' 
#' }
#'
#' @export
set_diagnosands <- function(x, diagnosands = default_diagnosands) {
  
  attr(x, "diagnosands") <- diagnosands

  x
}
