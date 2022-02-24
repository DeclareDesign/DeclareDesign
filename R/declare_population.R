#' Declare the size and features of the population
#' 
#' Deprecated. Please use declare_model instead.
#'
#' @inheritParams declare_internal_inherit_params
#' @return A potential outcomes declaration, which is a function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#' 
#' @keywords internal
#'
declare_population <- make_declarations(fabricate, "population")
