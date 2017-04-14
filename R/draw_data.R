

#' @export
draw_data <- function(design){
  design$data_function()
}

#' @export
get_estimates <- function(design){
  design$design_function()$estimates_df
}

#' @export
get_estimands <- function(design){
  design$design_function()$estimands_df
}
