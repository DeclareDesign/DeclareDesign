

#' @importFrom dplyr summarize_
#' @export
declare_diagnosands <- function(...) {
  diagnosands <- eval(substitute(alist(...)))

  diagnosand_function_internal <- function(data) {
    summarize_(data, .dots = diagnosands)
  }

  attributes(diagnosand_function_internal) <-
    list(call = match.call(), type = "diagnosand")

  return(diagnosand_function_internal)

}
