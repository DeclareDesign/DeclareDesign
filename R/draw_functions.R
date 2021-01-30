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
#' design <- 
#'   declare_model(
#'     N = 100, 
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ Z + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N, n = 75), legacy = FALSE) +
#'   declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' dat <- draw_data(design)
#' 
#' dat_no_sampling <- draw_data(design, end = 3)
#' 
#' draw_inquiries(design)
#' 
#' draw_estimates(design)
#'
#' @export
# draw_data <- function(design, end = length(design)) {
#   get_function_internal(design, data = -9, start = 1, end = end, function(x) TRUE)
# }
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
draw_inquiry <- function(...) apply_on_design_dots(draw_inquiries_single_design, ...)

#' @param ... A design or set of designs typically created using the + operator
#' @rdname draw_functions
#'
#' @export
draw_inquiries <- draw_inquiry

#' @rdname draw_functions
#' @export
draw_estimands <-  function(...){
  .Deprecated(new = "draw_estimands")
  draw_inquiries(...)
}

#' @rdname draw_functions
#' @export
draw_estimand <-  function(...){
  .Deprecated(new = "draw_estimand")
  draw_inquiry(...)
}

#' @rdname draw_functions
#'
#' @export
draw_estimates <- function(...) apply_on_design_dots(draw_estimates_single_design, ...)

draw_estimates_single_design <- function(design) {
  get_function_internal(  
    design, -9, 1, length(design), function(x) TRUE, 
    list("estimator" = vector("list", length(design))), "estimates_df", step_type = "estimator")
}

draw_inquiries_single_design <- function(design) {
  get_function_internal(
    design, -9, 1, length(design), function(x) TRUE, 
    list("inquiry" = vector("list", length(design))), "inquiries_df", step_type = "inquiry")
}

