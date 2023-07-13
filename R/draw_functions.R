#' Draw data, estimates, and inquiries from a design
#'  
#' @param design A design object, typically created using the + operator
#' @param data A data.frame object with sufficient information to get the data, estimates, inquiries, an assignment vector, or a sample.
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin with. By default all data steps are drawn, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish drawing data by.
#'
#' @name draw_functions
#' 
#' @examples 
#' 
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # Use draw_data to create a dataset using a design
#' dat <- draw_data(design)
#' 
#' # Use end argument to draw data up to a certain design component
#' dat_no_sampling <- draw_data(design, end = 3)
#' 
#' # Use draw_estimands to extract value of inquiry
#' draw_estimands(design)
#' 
#' # Use draw_estimates to extract value of estimator
#' draw_estimates(design)
#'
#' @export
draw_data <- function(design, data = NULL, start = 1, end = length(design)) {
  data_internal <- data
  if(is.null(data_internal)) {
    data_internal <- -9
  }
  get_function_internal(
    design, data_internal, start, end, function(x) attr(x, "causal_type") %in% "dgp")
}

#' @param ... A design or set of designs typically created using the + operator
#' @rdname draw_functions
#'
#' @export
draw_estimand <- function(...) apply_on_design_dots(draw_estimands_single_design, ...)

#' @param ... A design or set of designs typically created using the + operator
#' @rdname draw_functions
#'
#' @export
draw_estimands <- draw_estimand

#' @rdname draw_functions
#'
#' @export
draw_estimates <- function(...) apply_on_design_dots(draw_estimates_single_design, ...)

draw_estimates_single_design <- function(design) {
  get_function_internal(  
    design, -9, 1, length(design), function(x) TRUE, 
    list("estimator" = vector("list", length(design))), "estimates_df", step_type = "estimator")
}

draw_estimands_single_design <- function(design) {
  get_function_internal(
    design, -9, 1, length(design), function(x) TRUE, 
    list("inquiry" = vector("list", length(design))), "inquiries_df", step_type = "inquiry")
}

