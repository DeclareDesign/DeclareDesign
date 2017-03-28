

#' @importFrom magrittr "%>%"
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
#' @importFrom dplyr filter_ select_
#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_sampling <- function(..., sampling_function = randomizr::draw_rs,
                               sampling_probability_function = randomizr::obtain_inclusion_probabilities,
                               sampling_variable_name = "Z") {

  ## if you provide your own sampling_function and don't define your own sampling_probability_function
  ## then we don't want to use randomizr's obtain_condition_probabilities (it likely won't work by default)
  if (!(
    substitute(sampling_function) == "draw_rs" &
    getNamespaceName(environment(sampling_function)) == "randomizr"
  ) &
  (
    substitute(sampling_probability_function) == "obtain_inclusion_probabilities" &
    getNamespaceName(environment(sampling_probability_function)) == "randomizr"
  )) {
    sampling_probability_function <- NULL
  }

  sampling_dots <- sampling_probability_dots <- lazy_dots(...)
  sampling_mcall <- make_call(substitute(sampling_function),
                                sampling_dots)
  sampling_probability_mcall <- make_call(substitute(sampling_probability_function),
                                            sampling_probability_dots)

  argument_names_sampling_function <-
    names(formals(sampling_function))

  argument_names_sampling_probability_function <-
    names(formals(sampling_probability_function))

  sampling_function_options <- names(sampling_dots)
  sampling_probability_function_options <- names(sampling_probability_dots)

  sampling_function_internal <- function(data) {
    if ("N" %in% argument_names_sampling_function &
        !("N" %in% sampling_function_options)) {
      sampling_mcall$expr$N <- nrow(data)
    }
    data[, sampling_variable_name] <- lazy_eval(sampling_mcall, data = data)
    return(data)
  }

  sampling_probability_function_internal <- function(data) {
    ## if N is an option in your sampling_function and you don't provide it in ...
    ## then we add it for convenience to make things easier
    if ("N" %in% argument_names_sampling_probability_function &
        !("N" %in% sampling_probability_function_options)) {
      sampling_probability_mcall$expr$N <- nrow(data)
    }
    if ("sampling" %in% argument_names_sampling_probability_function)
      sampling_probability_mcall$expr$sampling <-
        data[, sampling_variable_name]
    data[, paste0(sampling_variable_name, "_inclusion_prob")] <-
      lazy_eval(sampling_probability_mcall, data = data)
    return(data)
  }


  sampling_function_return_internal <- function(data) {
    data %>%
      sampling_function_internal %>%
      sampling_probability_function_internal %>%
      filter_(paste0(sampling_variable_name, "== 1")) %>%
      select_(paste0("-", sampling_variable_name))
  }

  attributes(sampling_function_return_internal) <-
    list(
      call = match.call(),
      type = "sampling",
      sampling_variable_name = sampling_variable_name
    )

  return(sampling_function_return_internal)

}

