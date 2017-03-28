
#' @importFrom DDfabricate fabricate_data
#' @importFrom lazyeval lazy_dots make_call lazy_eval
#' @export
declare_population <- function(population_function = fabricate_data, ...) {
  dots <- lazyeval::lazy_dots(...)
  mcall <- lazyeval::make_call(substitute(population_function), dots)
  population_function_internal <- function() {
    lazyeval::lazy_eval(mcall)
  }
  attributes(population_function_internal) <-
         list(call = match.call(), type = "population")

  return(population_function_internal)
}
