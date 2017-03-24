
#' @importFrom magrittr "%>%"
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
#' @export
declare_sampling <- function(sampling_function = draw_rs,
                             sampling_probability_function = obtain_inclusion_probabilities,
                             sampling_variable_name = "S",
                             ...){

  ## if you provide your own sampling_function and don't define your own sampling_probability_function
  ## then we don't want to use randomizr's obtain_inclusion_probabilities (it likely won't work by default)
  if(!(substitute(sampling_function) == "draw_rs" &
       getNamespaceName(environment(sampling_function)) == "randomizr") &
     (substitute(sampling_probability_function) == "obtain_inclusion_probabilities" &
      getNamespaceName(environment(sampling_probability_function)) == "randomizr")){
    sampling_probability_function <- NULL
  }

  ## pull in the options for the sampling_function
  sampling_function_options <- eval(substitute(alist(...)))
  argument_names_sampling_function <- names(formals(sampling_function))

  ## create a function to take data and return a random sampling vector
  sampling_function_internal <- function(data){
    ## if N is an option in your sampling_function and you don't provide it in ...
    ## then we add it for convenience to make things easier
    if("N" %in% argument_names_sampling_function &
       !("N" %in% sampling_function_options)){
      sampling_function_options$N <- nrow(data)
    }

    data_environment <- list2env(data)

    ## the function runs the sampling_function using the options sent to ...
    ## and using the data sent to the function
    data[, sampling_variable_name] <- do.call(sampling_function, args = sampling_function_options, envir = data_environment)

    return(data)
  }

  sampling_probability_function_options <- eval(substitute(alist(...)))
  argument_names_sampling_probability_function <- names(formals(sampling_probability_function))


  sampling_probability_function_internal <- function(data){

    ## if N is an option in your sampling_function and you don't provide it in ...
    ## then we add it for convenience to make things easier
    if("N" %in% argument_names_sampling_probability_function &
       !("N" %in% sampling_probability_function_options)){
      sampling_probability_function_options$N <- nrow(data)
    }

    data_environment <- list2env(data)

    ## the function runs the sampling_function using the options sent to ...
    ## and using the data sent to the function
    if(!is.null(sampling_probability_function)){
      data[, paste0(sampling_variable_name, "_inclusion_prob")] <-
        do.call(sampling_probability_function, args = sampling_probability_function_options, envir = data_environment)
    }

    return(data)
  }

  sampling_function_return_internal <- function(data){
    data %>%
      sampling_function_internal %>%
      sampling_probability_function_internal %>%
      filter_(paste0(sampling_variable_name, "== 1")) %>%
      select_(paste0("-", sampling_variable_name))
  }

  attributes(sampling_function_return_internal) <- list(call = match.call(), type = "sampling")

  return(sampling_function_return_internal)
}
