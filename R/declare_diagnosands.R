
#' @importFrom dplyr summarize
#' @export
declare_diagnosands <- function(...) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(summarize)

  diagnosand_function_internal <- function(data) {
    args$.data <- data
    do.call(func, args = args, envir = env)
  }

  attributes(diagnosand_function_internal) <-
    list(call = match.call(), type = "diagnosand")

  return(diagnosand_function_internal)
}

