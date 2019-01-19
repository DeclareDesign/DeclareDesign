
#' @rdname post_design
#'
#' @export
draw_data <- function(design, end = length(design)) {
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }
  
  run_design_internal(design[1:end], results = list(current_df = 0))$current_df
}

#' @rdname post_design
#'
#' @export
draw_estimands <- function(...) apply_on_design_dots(draw_estimands_single_design, ...)

#' @rdname post_design
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
