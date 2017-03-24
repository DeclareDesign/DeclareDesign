

#' @importFrom DDestimate difference_in_means
#' @export
declare_estimator_ <-
  function(formula,
           estimator_function = difference_in_means,
           label = "my_estimator",
           estimand,
           ...) {
    options <- list(...)

    estimator_function_internal <- function(data) {

      # if it's in the formals
      options$data <- data

      options$formula <- formula

      value <- do.call(estimator_function, args = options)
      return(data.frame(estimator_label = label, value,
                        estimand_label = attributes(estimand)$label, stringsAsFactors = FALSE))
    }

    attributes(estimator_function_internal) <-
      list(call = match.call(), type = "estimator")

    return(estimator_function_internal)

  }





#' @importFrom DDestimate difference_in_means
#' @export
#'
declare_estimator <- function(formula, estimator_function = difference_in_means, label = "my_estimator",estimand, ...) {

  declare_estimator_(formula = formula,
                     estimator_function = estimator_function,
                     label = deparse(substitute(label)),
                     estimand = estimand,
                     ... = ...)

}
