
#' @rdname post_design
#'
#' @export
draw_data <- function(design) {
  run_design_internal(design, results = list(current_df = 0))$current_df
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
