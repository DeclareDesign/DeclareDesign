


#' Declare Estimand
#'
#' @param ... Arguments to the estimand function. For example, you might specify ATE = mean(Y_Z_1 - Y_Z_0), which would declare the estimand to be named ATE and to be the mean of the difference in the control and treatment potential outcome.
#' @param estimand_function A function that takes a data.frame as an argument and returns a data.frame with the estimand and a label. By default, the estimand function accepts an expression such as ATE = mean(Y_Z_1-Y_Z_0).
#' @param label An optional label to name the estimand, such as ATE. Typically, the label is inferred from how you specify the estimand in \code{...}, i.e. if you specify ATE = mean(Y_Z_1 - Y_Z_0) the estimand label will be ATE.
#'
#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @export
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   formula = Y ~ .25 * Z,
#'   condition_names = c(0, 1))
#'
#' df <- my_potential_outcomes(my_population())
#'
#' # Use the default estimand setup to
#' # declare an average treatment effect estimand
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimand(df)
#'
#' # Custom random assignment functions
#'
#' my_estimand_function <- function(data) {
#'   with(data, median(Y_Z_1 - Y_Z_0))
#' }
#' my_estimand_custom <- declare_estimand(
#'   estimand_function = my_estimand_function, label = medianTE)
#'
#' my_estimand_custom(df)
#'
declare_estimand <-
  function(...,
           estimand_function = estimand_function_default,
           label = NULL) {
    args <- eval(substitute(alist(...)))
    env <- freeze_environment(parent.frame())
    func <- eval(estimand_function)

    ## handles four uses cases of labeling so it's really easy to label without specifying
    ## would be great to clean up the next 10 lines
    label_internal <- NULL
    if (substitute(estimand_function) == "estimand_function_default" &
        from_package(estimand_function, "DeclareDesign")) {
      label_internal <- names(args)[1]
    }

    if (!exists("label_internal") | is.null(label_internal)) {
      label_internal <- substitute(label)
      if (!is.null(label_internal)) {
        label_internal <- as.character(label_internal)
      } else {
        label_internal <- "my_estimand"
      }
    }

    estimand_function_internal <- function(data) {
      args$data <- data
      value <- do.call(func, args = args, envir = env)
      data.frame(
        estimand_label = label_internal,
        estimand = value,
        stringsAsFactors = FALSE
      )
    }
    attributes(estimand_function_internal) <-
      list(call = match.call(),
           type = "estimand",
           label = label_internal)

    return(estimand_function_internal)
  }

#' @importFrom lazyeval lazy_eval lazy_dots
#' @export
estimand_function_default <- function(data, ..., subset = NULL) {
  options <- lazy_dots(...)
  if (length(options) > 1) {
    stop("Please only provide a single estimand to declare_estimand.")
  }

  condition_call <- substitute(subset)
  if (!is.null(condition_call)) {
    data <- data[eval(condition_call, data), , drop = FALSE]
  }

  lazy_eval(options[[1]], data = data)
}
