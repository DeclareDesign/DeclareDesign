

#' @importFrom magrittr "%>%"
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_assignment <- function(..., assignment_function = randomizr::conduct_ra,
                               assignment_probability_function = randomizr::obtain_condition_probabilities,
                               assignment_variable_name = "Z") {

  ## if you provide your own assignment_function and
  ## don't define your own assignment_probability_function
  ## then we don't want to use randomizr's obtain_condition_probabilities
  ## (it likely won't work by default)

  if (!(
    substitute(assignment_function) == "conduct_ra" &
    from_package(assignment_function, "randomizr")
  ) &
  (
    substitute(assignment_probability_function) == "obtain_condition_probabilities" &
    from_package(assignment_probability_function, "randomizr")
  )) {
    assignment_probability_function <- NULL
  }

  assignment_dots <- assignment_probability_dots <- lazy_dots(...)
  assignment_mcall <- make_call(substitute(assignment_function),
                                assignment_dots)
  assignment_probability_mcall <- make_call(substitute(assignment_probability_function),
                                            assignment_probability_dots)

  argument_names_assignment_function <-
    names(formals(assignment_function))

  argument_names_assignment_probability_function <-
    names(formals(assignment_probability_function))

  assignment_function_options <- names(assignment_dots)
  assignment_probability_function_options <- names(assignment_probability_dots)

  assignment_function_internal <- function(data) {
    if ("N" %in% argument_names_assignment_function &
        !("N" %in% assignment_function_options)) {
      assignment_mcall$expr$N <- nrow(data)
    }
    data[, assignment_variable_name] <- lazy_eval(assignment_mcall, data = data)
    return(data)
  }

  assignment_probability_function_internal <- function(data) {
    if ("assignment" %in% argument_names_assignment_probability_function)
      assignment_probability_mcall$expr$assignment <-
        data[, assignment_variable_name]
    data[, paste0(assignment_variable_name, "_condition_prob")] <-
      lazy_eval(assignment_probability_mcall, data = data)
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

