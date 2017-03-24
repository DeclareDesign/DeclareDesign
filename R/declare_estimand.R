

#' @export
declare_estimand <-
  function(...,
           estimand_function = default_estimand_function,
           label = "my_estimand") {
    options <- eval(substitute(alist(...)))

    label <- deparse(substitute(label))

    estimand_function_internal <- function(data) {
      options$data <- data
      value <- do.call(estimand_function, args = options)
      return(data.frame(
        estimand_label = label,
        estimand = value,
        stringsAsFactors = FALSE
      ))
    }

    attributes(estimand_function_internal) <-
      list(call = match.call(),
           type = "estimand",
           label = label)

    return(estimand_function_internal)

  }

#' @export
default_estimand_function <- function(data, estimand) {
  estimand <- substitute(estimand)
  data_environment <- list2env(data)
  eval(estimand, envir = data_environment)
}
