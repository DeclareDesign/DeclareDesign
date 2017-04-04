

#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_estimand <- function(..., label = my_estimand, estimand_function = default_estimand_function) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(estimand_function)
  label <- as.character(substitute(label))
  if (!("data" %in% names(formals(func)))) {
    stop("Please choose potential_outcomes_function with a data argument.")
  }
  estimand_function_internal <- function(data) {
    args$data <- data
    value <- do.call(func, args = args, envir = env)
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
