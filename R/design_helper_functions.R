


#' Draw Data from a Design
#'
#' @param design A design created by \code{\link{declare_design}}.
#'
#' @export
draw_data <- function(design) {
  design$data_function()
}

#' Get Estimates from a Design
#'
#' @param design A design created by \code{\link{declare_design}}.
#'
#' @return a \code{data.frame} of estimates calculated from a designs estimators.
#'
#' @export
get_estimates <- function(design) {
  design$design_function()$estimates_df
}

#' Get Estimands from a Design
#'
#' @param design A design created by \code{\link{declare_design}}.
#'
#' @return a \code{data.frame} of estimands.
#'
#' @export
get_estimands <- function(design) {
  design$design_function()$estimands_df
}
