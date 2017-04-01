#' @importFrom DDfabricate fabricate_data
#' @importFrom lazyeval lazy_dots make_call lazy_eval
#' @export
declare_population <- function(population_function = DDfabricate::fabricate_data, ...) {
  dots <- lazy_dots(...)
  mcall <- make_call(substitute(population_function), dots)
  population_function_internal <- function() {
    lazy_eval(mcall)
  }
  attributes(population_function_internal) <-
    list(call = match.call(), type = "population")

  return(population_function_internal)
}
