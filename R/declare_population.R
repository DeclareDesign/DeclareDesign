
#' @importFrom DDfabricate fabricate_data
#' @importFrom rlang eval_tidy dots_quos quos UQ
#' @export
declare_population <- function(##population_function = DDfabricate::fabricate_data,
  population_function = default_population_function,
  ...) {

  args <- dots_quos(...)
  if ("data" %in% names(formals(population_function))) {
    ## if data is an argument of population_function
    args <- c(args, quos(data = data))
  }

  population_call_internal <- dots_quos(
    UQ(population_function)(!!!args))

  population_function_internal <- function(data = NULL) {
    eval_tidy(population_call_internal, data = list(data = data))
  }

  attributes(population_function_internal) <-
         list(call = match.call(), type = "population")

  return(population_function_internal)
}

default_population_function <- function(N) {
  data.frame(u = rnorm(N))
}
