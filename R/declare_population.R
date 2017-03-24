

#' @importFrom frontloadr simulate_data level
#' @export
declare_population <-
  function(..., population_function = simulate_data) {
    ## pull in the options for the population_function
    population_function_options <- eval(substitute(alist(...)))
    argument_names_population_function <-
      names(formals(population_function))

    ## create a function to return a population
    population_function_internal <- function() {
      ## the function runs the population_function using the options sent to ...
      do.call(population_function, args = population_function_options)
    }

    attributes(population_function_internal) <-
      list(call = match.call(), type = "population")

    return(population_function_internal)
  }
