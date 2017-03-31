

#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_estimand <- function(..., label = my_estimand, estimand_function = default_estimand_function) {
  dots <- lazy_dots(...)
  mcall <- make_call(substitute(estimand_function), dots)
  label <- as.character(substitute(label))
  estimand_function_internal <- function(data) {
    mcall$expr$data <- data
    value <- lazy_eval(mcall)
    data.frame(estimand_label = label, estimand = value, stringsAsFactors = FALSE)
  }
  attributes(estimand_function_internal) <-
    list(call = match.call(), type = "estimand")

  return(estimand_function_internal)
}

#' @export
default_estimand_function <- function(data, estimand){
  estimand <- parse(text = deparse(substitute(estimand)))
  data_environment <- list2env(data)
  eval(estimand, envir = data_environment)
}
