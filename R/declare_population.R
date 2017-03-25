
#' @export
declare_population <- function(..., N = NULL, population_function = fabricate_data){

  options <- eval(substitute(alist(...)))

  # if it's levels, do one thing:
  options_text <- paste(substitute(options))

  # revenge of the JANK TOWN, i.e., check if all the options are level calls.
  if (all(sapply(options_text, function(x)
    startsWith(x, "level(")))) {
    # do levels stuff, not sure what

    return(do.call(declare_population_, args = c(list(N = N), as.list(options_text))))

  } else {
    # change the ones that are calls to character strings for fabricate_data_
    is_call <- sapply(options, class) == "call"

    options[is_call] <-
      as.list(paste0(names(options[is_call]), " = ", paste(options[is_call])))
    names(options)[is_call] <- ""

    do.call(declare_population_, args = c(list(N = N), as.list(options)))

  }
}


#' @importFrom DDfabricate fabricate_data_ level
#' @export
declare_population_ <-
  function(..., N = NULL, population_function = fabricate_data_) {
    ## pull in the options for the population_function
    options <- list(...)

    ## create a function to return a population
    population_function_internal <- function() {
      ## the function runs the population_function using the options sent to ...
      do.call(population_function, args = c(list(N = N), options))
    }

    attributes(population_function_internal) <-
      list(call = match.call(), type = "population")

    return(population_function_internal)
  }




