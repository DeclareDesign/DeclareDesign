#' Declare an Estimator
#'
#' @param ... Arguments to the estimand function. For example, you could specify the formula for your estimator, i.e., formula = Y ~ Z + age.
#' @param handler the handler function
#' @param label An optional label to name the estimator, such as DIM.
#' @param data a data.frame
#'
#' @export
#' @importFrom estimatr difference_in_means
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimator and associated statistics.
#'
#' @section Custom Estimators:
#'
#' \code{estimator_functions} implementations should be tidy (accept and return a data.frame)
#'
#' \code{model} implementations should at the miminum provide S3 methods for \code{summary} and \code{confint}.
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
#'                    model = estimatr::lm_robust,
#'                    coefficient_name = "Z",
#'                    estimand = my_estimand)
#'
#' my_estimator_lm(df)
#'
#' # Use R's built-in lm function via model
#'
#' estimator_lm <- declare_estimator(Y ~ Z, model = lm)
#'
#' # Run a probit regression using glm via model
#'
#' estimator_probit <-
#'   declare_estimator(
#'     Y ~ Z,
#'     model = glm,
#'     family = binomial(link = "probit"),
#'     coefficient_name = "Z"
#'  )
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
declare_estimator <-make_declarations(estimator_handler, step_type="estimator", causal_type="estimator", default_label="my_estimator")

#' @param estimator_function A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval) and a label.
#' @param model A model function, e.g. lm or glm. If model is specified, the estimator_function argument is ignored.By default, the model is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param coefficient_name A character vector of coefficients that represent quantities of interest, i.e. Z. Only relevant when a \code{model} is chosen or for some \code{estimator_function}'s such as \code{difference_in_means} and \code{lm_robust}.
#' @param estimand An estimand object created using \code{\link{declare_estimand}}. Estimates from this estimator function will be associated with the estimand, for example for calculating the bias and coverage of the estimator.
#' @rdname declare_estimator
estimator_handler <- function(data, ...,
                              model = estimatr::difference_in_means,
                              estimator_function = NULL,
                              coefficient_name = Z,
                              estimand = NULL, label) {
  model <- if(is.null(estimator_function)) model else NULL

  # coefficient_name <- to_char_except_null(substitute(coefficient_name))
  coefficient_name <- reveal_nse_helper(substitute(coefficient_name))

  if (is.null(model) == is.null(estimator_function)) {
    stop("Please provide either an estimator function or a model.")
  } else if(!is.null(model)) {
    lbl <- 'a model'
    func <- match.fun(model)
    clean <- fit2tidy #todo could generify this...
  } else if(!is.null(estimator_function)) {
    lbl <- 'an estimator'
    func <- match.fun(estimator_function)
    clean <- function(x,y)x # no work needed after evaluated
  }

  if (!("data" %in% names(formals(func)))) {
    stop("Please provide ", lbl, " function with a data argument.")
  }

  estimand_label <- switch(class(estimand), "character"=estimand, "function"=attributes(estimand)$label)

  # estimator_function_internal <- function(data) {
    args <- quos(...)
    args$data <- data
    if ("coefficient_name" %in% names(formals(func))) {
      args$coefficient_name <- coefficient_name
    }
    # results <- do.call(func, args = args)
    # results <- eval_tidy(quo(func(!!!args)))
    W <- quo(func(!!!args))
    results <- eval(quo_expr(W))
    results <- clean(results, coefficient_name) # fit2tidy if a model function, ow I
    return_data <-
      data.frame(estimator_label = label,
                 results,
                 stringsAsFactors = FALSE)

    return_data[['estimand_label']] <- estimand_label

    return_data
  }

#   attributes(estimator_function_internal) <-
#     list(call = match.call(),
#          step_type = "estimator",
#          causal_type = "estimator",
#          label = label,
#          estimand_label = estimand_label)
#
#   return(estimator_function_internal)
# }

fit2tidy <- function(fit, coefficient_name = NULL) {
  summ <- summary(fit)$coefficients
  summ <-
    summ[, tolower(substr(colnames(summ), 1, 3)) %in% c("est", "std", "pr("), drop = FALSE]
  ci <- suppressMessages(as.data.frame(confint(fit)))
  return_data <-
    data.frame(coefficient_name = rownames(summ),
               summ,
               ci,
               stringsAsFactors = FALSE, row.names = NULL)
  colnames(return_data) <- c("coefficient_name","est", "se", "p", "ci_lower", "ci_upper")


  if (!is.null(coefficient_name)) {
    return_data <- return_data[return_data$coefficient_name %in% coefficient_name, ,drop = FALSE]
  }

  return_data
}

#todo migrate to utils.R
to_char_except_null <- function(x){
  if(is.null(x)) NULL else if(is_quosure(x)) as.character(x[[2]]) else as.character(x)
}
