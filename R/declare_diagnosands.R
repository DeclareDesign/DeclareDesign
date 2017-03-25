

#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @importFrom dplyr summarize_
#' @export
declare_diagnosands <- function(...) {
  dots <- lazy_dots(...)
  mcall <- make_call(substitute(summarize), args = dots)
  diagnosand_function_internal <- function(data) {
    mcall$expr$.data <- data
    lazy_eval(mcall)
  }
  attributes(diagnosand_function_internal) <-
    list(call = match.call(), type = "diagnosand")

  return(diagnosand_function_internal)
}

