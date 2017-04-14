
#' @importFrom DDestimate difference_in_means
#' @export
declare_estimator <- function(..., label = my_estimator,
                              estimator_function = DDestimate::difference_in_means,
                              estimand = NULL) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(estimator_function)
  label <- as.character(substitute(label))
  if (!("data" %in% names(formals(func)))) {
    stop("Please choose potential_outcomes_function with a data argument.")
  }
  estimator_function_internal <- function(data) {
    args$data <- data
    results <- do.call(func, args = args, envir = env)
    return_data <- data.frame(estimator_label = label, results, stringsAsFactors = FALSE)
    if (!is.null(estimand)) {
      return_data$estimand_label <- attributes(estimand)$label
    }
    return_data
  }
  attributes(estimator_function_internal) <-
    list(call = match.call(), type = "estimator")

  return(estimator_function_internal)
}
