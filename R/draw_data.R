

#' @export
draw_data <- function(design){
  design$data_function()
}

#' @export
simulate_design <- function(design){
  design$design_function()
}
