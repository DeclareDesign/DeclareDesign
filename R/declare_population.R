#' @importFrom fabricatr fabricate_data
#' @export
declare_population <- function(..., population_function = fabricatr::fabricate_data) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(population_function)
  population_function_internal <- function(data = NULL) {
    if (!is.null(data) & ("data" %in% names(formals(func)))) {
      args$data <- data
    }
    do.call(func, args = args, envir = env)
  }
  attributes(population_function_internal) <-
    list(call = match.call(), type = "population")

  return(population_function_internal)
}
