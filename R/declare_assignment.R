
#' @export
declare_assignment <- function(..., assignment_function = assignment_function_default) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(assignment_function)
  if (!("data" %in% names(formals(func)))) {
    stop("Please choose an assignment_function with a data argument.")
  }
  assignment_function_internal <- function(data) {
    args$data <- data
    do.call(func, args = args, envir = env)
  }
  attributes(assignment_function_internal) <-
    list(call = match.call(), type = "assignment")

  return(assignment_function_internal)
}

#' @importFrom randomizr conduct_ra obtain_condition_probabilities
assignment_function_default <- function(data, ..., assignment_variable_name = "Z"){

  options <- eval(substitute(alist(...)))
  options$N <- nrow(data)
  data[,assignment_variable_name] <-
    do.call(what = conduct_ra, args = options, envir = list2env(data))

  options$assignment <- data[,assignment_variable_name]

  data[,paste0(assignment_variable_name, "_cond_prob")] <-
    do.call(what = obtain_condition_probabilities, args = options, envir = list2env(data))

  return(data)

}

