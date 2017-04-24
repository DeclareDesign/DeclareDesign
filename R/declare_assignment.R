
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

#' @importFrom lazyeval make_call lazy_eval as.lazy
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
assignment_function_default <- function(data, ..., assignment_variable_name = "Z"){

  ## draw assignment

  options <- lazy_dots(...)

  if(length(options) > 0){
    options$N <- as.lazy(nrow(data), env = options[[1]]$env)
  } else {
    options$N <- as.lazy(nrow(data))
  }

  mcall <- make_call(quote(randomizr::conduct_ra), args = options)

  data[,assignment_variable_name] <- lazy_eval(mcall, data = data)

  ## obtain condition probabilities

  options <- lazy_dots(...)

  if(length(options) > 0){
    options$assignment <- as.lazy(assignment_variable_name, env = options[[1]]$env)
  } else {
    options$assignment <- as.lazy(assignment_variable_name)
  }



  mcall <- make_call(quote(randomizr::obtain_condition_probabilities), args = options)

  data[,paste0(assignment_variable_name, "_cond_prob")] <- lazy_eval(mcall, data = data)

  return(data)

}

