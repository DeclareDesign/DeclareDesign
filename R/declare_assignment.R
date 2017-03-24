










#' @importFrom magrittr "%>%"
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
#' @importFrom dplyr filter_ select_
#' @export
declare_assignment <- function(assignment_function = conduct_ra,
                               assignment_probability_function = obtain_condition_probabilities,
                               assignment_variable_name = "Z",
                               ...) {
  ## if you provide your own assignment_function and don't define your own assignment_probability_function
  ## then we don't want to use randomizr's obtain_condition_probabilities (it likely won't work by default)
  if (!(
    substitute(assignment_function) == "conduct_ra" &
    getNamespaceName(environment(assignment_function)) == "randomizr"
  ) &
  (
    substitute(assignment_probability_function) == "obtain_condition_probabilities" &
    getNamespaceName(environment(assignment_probability_function)) == "randomizr"
  )) {
    assignment_probability_function <- NULL
  }

  ## pull in the options for the assignment_function
  assignment_function_options <- eval(substitute(alist(...)))
  argument_names_assignment_function <-
    names(formals(assignment_function))

  ## create a function to take data and return a random assignment vector
  assignment_function_internal <- function(data) {
    ## if N is an option in your assignment_function and you don't provide it in ...
    ## then we add it for convenience to make things easier
    if ("N" %in% argument_names_assignment_function &
        !("N" %in% assignment_function_options)) {
      assignment_function_options$N <- nrow(data)
    }

    data_environment <- list2env(data)

    ## the function runs the assignment_function using the options sent to ...
    ## and using the data sent to the function
    data[, assignment_variable_name] <-
      do.call(assignment_function, args = assignment_function_options, envir = data_environment)

    return(data)
  }

  assignment_probability_function_options <-
    eval(substitute(alist(...)))
  argument_names_assignment_probability_function <-
    names(formals(assignment_probability_function))

  assignment_probability_function_internal <- function(data) {
    if ("assignment" %in% argument_names_assignment_probability_function)
      assignment_probability_function_options$assignment <-
        data[, assignment_variable_name]

    data_environment <- list2env(data)

    ## the function runs the assignment_function using the options sent to ...
    ## and using the data sent to the function
    if (!is.null(assignment_probability_function)) {
      data[, paste0(assignment_variable_name, "_condition_prob")] <-
        do.call(assignment_probability_function,
                args = assignment_probability_function_options,
                envir = data_environment)
    }

    return(data)
  }

  assignment_function_return_internal <- function(data) {
    data %>%
      assignment_function_internal %>%
      assignment_probability_function_internal
  }

  attributes(assignment_function_return_internal) <-
    list(
      call = match.call(),
      type = "assignment",
      assignment_variable_name = assignment_variable_name
    )

  return(assignment_function_return_internal)
}
