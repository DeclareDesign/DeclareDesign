#' Draw data, estimates, and estimands from a design
#'  
#' @param design A design object, typically created using the + operator
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish drawing data by.
#'
#' @name draw_functions
#' 
#' @examples 
#' 
#' design <- declare_population(N = 100, u = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ Z + u) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(n = 75) +
#'   declare_assignment(m = 50) +
#'   declare_reveal(Y, Z) +
#'   declare_estimator(Y ~ Z, estimand = "ATE")
#' 
#' dat <- draw_data(design)
#' 
#' dat_no_sampling <- draw_data(design, end = 3)
#' 
#' draw_estimands(design)
#' 
#' draw_estimates(design)
#'
#' @export
draw_data <- function(design, end = length(design)) {

  check_get_draw_function_inputs(design = design, end = end, type = "draw_data")
  
  run_design_internal.design(design[1:end], results = list(current_df = 0))$current_df
  
}

#' @param ... A design or set of designs typically created using the + operator
#' @rdname draw_functions
#'
#' @export
draw_estimands <- function(...) apply_on_design_dots(draw_estimands_single_design, ...)

#' @rdname draw_functions
#'
#' @export
draw_estimates <- function(...) apply_on_design_dots(draw_estimates_single_design, ...)

draw_estimates_single_design <- function(design) {
  results <- list("estimator" = vector("list", length(design)))
  run_design_internal(design, results = results)$estimates_df
}

draw_estimands_single_design <- function(design) {
  results <- list("estimand" = vector("list", length(design)))
  run_design_internal(design, results = results)$estimands_df
}
