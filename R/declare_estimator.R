#' Declare an Estimator
#'
#' @param ... Arguments to the estimation function. For example, you could specify the formula for your estimator, i.e., formula = Y ~ Z + age.
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
#' my_estimator_function <- function(data){
#'   data.frame(est = with(data, mean(Y)))
#' }
#'
#' my_estimator_custom <-
#'   declare_estimator(handler = custom_estimator(my_estimator_function),
#'                     estimand = my_estimand)
#'
#' my_estimator_custom(df)
#'
declare_estimator <- make_declarations(estimator_handler_default, step_type="estimator", causal_type="estimator", default_label="my_estimator")

#' \code{custom_estimator} takes an estimator function, and returns one which has standard labelling options.
#'
#' @param estimator_function A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval) and a label.
#' @rdname declare_estimator
#' @export
custom_estimator <- function(estimator_function){

  if (!("data" %in% names(formals(estimator_function)))) {
    stop("Must provide a `estimator_function` function with a data argument.")
  }


  f <- function(data, ..., estimand=NULL, label) {

    ret <- data.frame(
      estimator_label = label,
      estimator_function(data, ...),
      stringsAsFactors = FALSE
    )

    estimand_label <- get_estimand_label(estimand)
    if(length(estimand_label) > 0) {
      ret <- cbind(ret, estimand_label=estimand_label, row.names=NULL, stringsAsFactors=FALSE)
    }
    ret

  }

  attributes(f) <- attributes(estimator_function)

  f
}




#' @param model A model function, e.g. lm or glm. By default, the model is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param coefficient_name A character vector of coefficients that represent quantities of interest, i.e. Z. Only relevant when a \code{model} is chosen or for some \code{estimator_function}'s such as \code{difference_in_means} and \code{lm_robust}.
#' @rdname declare_estimator
estimator_handler <-
  function(data, ...,
    model = estimatr::difference_in_means,
    coefficient_name = Z)
  {

    coefficient_name <- reveal_nse_helper(substitute(coefficient_name))

    # estimator_function_internal <- function(data) {
    args <- quos(...)
    W <- quo(model(!!!args, data=data))
    results <- eval(quo_expr(W))

    results <- fit2tidy(results, coefficient_name)

    results
}

validation_fn(estimator_handler) <-  function(ret, dots, label){
  if("model" %in% names(dots)) {
    model <- eval_tidy(dots$model)
    if(!is.function(model) || ! "data" %in% names(formals(model))){
      stop(simpleError("Must provide a function for `model` which takes a `data` argument.", call = attr(ret, "call")))
    }
  }
  ret
}

#' @param estimand a declare_estimand step object, or a character label, or a list of either
#' @rdname declare_estimator
estimator_handler_default <- custom_estimator(estimator_handler)

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


  if (is.character(coefficient_name)) {
    return_data <- return_data[return_data$coefficient_name %in% coefficient_name, ,drop = FALSE]
  }

  return_data
}

get_estimand_label <- function(estimand){
  force(estimand) # no promise nonsense when we look at it
  switch(class(estimand)[1],
         "character"=estimand,
         "design_step"=attributes(estimand)$label,
         "list"=vapply(estimand, get_estimand_label, NA_character_), #note recursion here
         NULL=NULL,
         warning("Did not match class of `estimand`")
  )
}

