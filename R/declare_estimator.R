

#' @importFrom DDestimate difference_in_means
#' @export
declare_estimator <-
  function(...,
           estimator_function = difference_in_means,
           label = "my_estimator",
           estimand) {
    options <- eval(substitute(alist(...)))
    label <- deparse(substitute(label))

    estimator_function_internal <- function(data) {
      options$data <- data
      value <- do.call(estimator_function, args = options)
      return(
        data.frame(
          estimator_label = label,
          value,
          estimand_label = attributes(estimand)$label,
          stringsAsFactors = FALSE
        )
      )
    }

    attributes(estimator_function_internal) <-
      list(call = match.call(), type = "estimator")

    return(estimator_function_internal)

  }
