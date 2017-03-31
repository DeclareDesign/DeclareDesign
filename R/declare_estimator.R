

#' @importFrom DDestimate difference_in_means
#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_estimator <- function(..., label = my_estimator, estimator_function = DDestimate::difference_in_means) {
  dots <- lazy_dots(...)
  mcall <- make_call(substitute(estimator_function), dots)
  label <- as.character(substitute(label))
  estimator_function_internal <- function(data) {
    mcall$expr$data <- data
    results <- lazy_eval(mcall)
    data.frame(estimator_label = label, results, stringsAsFactors = FALSE)
  }
  attributes(estimator_function_internal) <-
    list(call = match.call(), type = "estimator")

  return(estimator_function_internal)
}
