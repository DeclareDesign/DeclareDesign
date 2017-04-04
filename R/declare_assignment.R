
from_package <- function(func, package){
  func_package <- tryCatch(getNamespaceName(environment(func)), error = function(e) NULL)
  ifelse(is.null(func_package), FALSE, func_package == package)
}


#' @importFrom magrittr "%>%"
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
#' @export
declare_assignment <- function(..., assignment_function = randomizr::conduct_ra,
                             assignment_probability_function = randomizr::obtain_condition_probabilities,
                             assignment_variable_name = "Z") {

  ## if you provide your own assignment_function and don't define your own assignment_probability_function
  ## then we don't want to use randomizr's obtain_condition_probabilities (it likely won't work by default)
  if (!(
    substitute(assignment_function) == "draw_rs" &
    from_package(assignment_function, "randomizr")
  ) &
  (
    substitute(assignment_probability_function) == "obtain_condition_probabilities" &
    from_package(assignment_probability_function, "randomizr")
  )) {
    assignment_probability_function <- NULL
  }

  env <- freeze_environment(parent.frame())

  assignment_args <- eval(substitute(alist(...)))
  assignment_function <- eval(assignment_function)

  assignment_probability_args <- eval(substitute(alist(...)))
  assignment_probability_function <- eval(assignment_probability_function)

  assignment_function_options <- names(assignment_args)
  assignment_probability_function_options <- names(assignment_probability_args)

  assignment_function_internal <- function(data) {
    if ("N" %in% names(formals(assignment_function)) &
        !("N" %in% assignment_function_options)) {
      assignment_args$N <- nrow(data)
    }
    data[, assignment_variable_name] <-
      do.call(assignment_function, args = assignment_args, envir = list2env(data))
    return(data)
  }

  argument_names_assignment_probability_function <-
    names(formals(assignment_probability_function))
  assignment_probability_function_internal <- function(data) {
    ## if N is an option in your assignment_function and you don't provide it in ...
    ## then we add it for convenience to make things easier
    if ("N" %in% argument_names_assignment_probability_function &
        !("N" %in% assignment_probability_function_options)) {
      assignment_probability_args$N <- nrow(data)
    }
    if ("assignment" %in% argument_names_assignment_probability_function)
      assignment_probability_args$assignment <-
        data[, assignment_variable_name]
    data[, paste0(assignment_variable_name, "_condition_prob")] <-
      do.call(assignment_probability_function,
              args = assignment_probability_args, envir = list2env(data))
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

