

#' @importFrom DDestimate difference_in_means
#' @export
declare_estimator_ <-
  function(formula,
           estimator_function = difference_in_means,
           label = "my_estimator",
           estimand = NULL,
           ...) {
    options <- list(...)

    estimator_function_internal <- function(data) {

      options$data <- data

      if(!is.null(formula) & "formula" %in%  names(formals(estimator_function)))
        options$formula <- stats::formula(unclass(formula))
      ##options$formula <- formula

      results <- do.call(estimator_function, args = options)
      return_object <- data.frame(estimator_label = label, results, stringsAsFactors = FALSE)

      if(!is.null(estimand)){
       return_object$estimand_label <- as.character(attributes(estimand)$label)
      }

      return(return_object)
    }

    attributes(estimator_function_internal) <-
      list(call = match.call(), type = "estimator")

    return(estimator_function_internal)

  }





#' @importFrom DDestimate difference_in_means
#' @export
#'
declare_estimator <- function(formula, estimator_function = difference_in_means, label = "my_estimator",estimand = NULL, ...) {

  declare_estimator_(formula = formula,
                     estimator_function = estimator_function,
                     label = deparse(substitute(label)),
                     estimand = estimand,
                     ... = ...)

}
