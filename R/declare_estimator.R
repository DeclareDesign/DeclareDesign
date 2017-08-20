
#' Declare an Estimator
#'
#' @param ... Arguments to the estimand function. For example, you could specify the formula for your estimator, i.e., formula = Y ~ Z + age.
#'
#' @param estimator_function A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval) and a label. By default, the estimator function is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param estimand An estimand object created using \code{\link{declare_estimand}}. Estimates from this estimator function will be associated with the estimand, for example for calculating the bias and coverage of the estimator.
#' @param label An optional label to name the estimator, such as DIM.
#'
#' @export
#' @importFrom estimatr difference_in_means
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimator and associated statistics.
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   formula = Y ~ .25 * Z,
#'   condition_names = c(0, 1))
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' design <- declare_design(
#'  my_population, my_potential_outcomes,
#'  my_estimand, my_assignment, reveal_outcomes
#' )
#'
#' df <- draw_data(design)
#'
#' my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' my_estimator_dim(df)
#'
#' # Use the linear regression (lm) function
#' # with robust standard errors from the estimatr package
#'
#' my_estimator_lm <-
#'  declare_estimator(Y ~ Z,
#'                    estimator_function = estimatr::lm_robust,
#'                    coefficient_name = "Z",
#'                    estimand = my_estimand)
#'
#' my_estimator_lm(df)
#'
#' # Use a custom estimator function
#'
#' my_estimator_function <- function(formula, data){
#'   data.frame(est = with(data, mean(Y)))
#' }
#'
#' my_estimator_custom <-
#'   declare_estimator(Y ~ Z,
#'                     estimator_function = my_estimator_function,
#'                     estimand = my_estimand)
#'
#' my_estimator_custom(df)
#'
declare_estimator <- function(...,
                              estimator_function = estimatr::difference_in_means,
                              estimand = NULL,
                              label = my_estimator) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(estimator_function)
  label <- as.character(substitute(label))

  if (!("data" %in% names(formals(func)))) {
    stop("Please provide an estimator function with a data argument.")
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
    list(call = match.call(), type = "estimator", label = label)

  return(estimator_function_internal)
}
