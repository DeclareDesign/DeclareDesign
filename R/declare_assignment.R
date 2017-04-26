
#' Declare Assignment Procedure
#'
#' @param ... Arguments to the assignment function.
#' @param assignment_function A function that takes a data.frame, adds an assignment variable and optionally assignment probabilities or other relevant quantities, and returns a data.frame. By default, the assignment_function uses the \link{randomizr} functions \code{\link{conduct_ra}} and \code{\link{obtain_condition_probabilities}} to conduct random assignment and obtain the probabilities of assignment to each condition.
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame with additional columns appended including an assignment variable and (optionally) probabilities of assignment.
#' @export
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#' df <- my_population()
#'
#' # Complete random assignment using randomizr
#' # use any arguments you would use in conduct_ra.
#'
#' my_assignment <- declare_assignment(m = 50)
#' df <- my_assignment(pop)
#' head(df)
#' table(df$Z)
#'
#' # Custom random assignment functions
#'
#' df <- my_population()
#'
#' my_assignment_function <- function(data) {
#'    data$Z <- rbinom(n = nrow(data),
#'    size = 1,
#'    prob = 0.5)
#'    data
#'    }
#'
#' my_assignment_custom <- declare_assignment(
#'    assignment_function = my_assignment_function)
#'
#' df <- my_assignment_custom(df)
#' head(df)
#' table(df$Z)
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

  if (from_package(assignment_function, "DeclareDesign") &
      substitute(assignment_function) == "assignment_function_default") {

    randomizr_summary <- function(data){
      args$N <- nrow(data)
      args$assignment_variable_name <- NULL

      ra_declaration <- do.call(randomizr::declare_ra, args = args, envir = list2env(data))

      return(print(ra_declaration))
    }

    attributes(assignment_function_internal)$summary_function <- randomizr_summary

  }

  return(assignment_function_internal)
}

#' @importFrom lazyeval make_call lazy_eval as.lazy
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
assignment_function_default <- function(data, ..., assignment_variable_name = "Z"){

  ## draw assignment

  options <- lazy_dots(...)

  if (length(options) > 0) {
    options$N <- as.lazy(nrow(data), env = options[[1]]$env)
  } else {
    options$N <- as.lazy(nrow(data))
  }

  mcall <- make_call(quote(randomizr::conduct_ra), args = options)

  data[,assignment_variable_name] <- lazy_eval(mcall, data = data)

  ## obtain condition probabilities

  options <- lazy_dots(...)

  if (length(options) > 0) {
    options$assignment <- as.lazy(assignment_variable_name, env = options[[1]]$env)
  } else {
    options$assignment <- as.lazy(assignment_variable_name)
  }

  mcall <- make_call(quote(randomizr::obtain_condition_probabilities), args = options)

  data[,paste0(assignment_variable_name, "_cond_prob")] <- lazy_eval(mcall, data = data)

  return(data)

}

