#' Declare an Estimator
#'
#' @inheritParams declare_internal_inherit_params
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
#'   conditions = c(0, 1))
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' my_reveal <- declare_reveal()
#'
#' design <- declare_design(
#'  my_population, my_potential_outcomes,
#'  my_estimand, my_assignment, my_reveal
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
#'                    coefficients = "Z",
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
#'     coefficients = "Z"
#'  )
#'
#' # Use a custom estimator function
#'
#' my_estimator_function <- function(data){
#'   data.frame(est = with(data, mean(Y)))
#' }
#'
#' my_estimator_custom <-
#'   declare_estimator(handler = tidy_estimator(my_estimator_function),
#'                     estimand = my_estimand)
#'
#' my_estimator_custom(df)
#'
declare_estimator <- make_declarations(estimator_handler, step_type="estimator", causal_type="estimator", default_label="my_estimator")

#' \code{tidy_estimator} takes an untidy estimation function, and returns a tidy handler which accepts standard labelling options.
#'
#' @param estimator_function A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval) and a label.
#' @rdname declare_estimator
#' @export
#' @importFrom rlang UQ
tidy_estimator <- function(estimator_function){

  if (!("data" %in% names(formals(estimator_function)))) {
    stop("Must provide a `estimator_function` function with a data argument.")
  }


  f <- function(data, ..., estimand=NULL, label) {

    calling_args <- names(match.call(expand.dots = FALSE)) %i% names(formals(estimator_function))

    dots <- if("..." %in% calling_args) quos(...) else list()

    calling_args <- setdiff(calling_args, c("", "data", "..."))

    for(e in calling_args) {
      dots[[e]] <- do.call(enquo, list(as.symbol(e))) # this *should* retrieve coefficient names as quosure. IDK
    }

    ret <- eval_tidy(quo(estimator_function(data, !!!dots)))

    ret <- data.frame(
      estimator_label = label,
      ret,
      stringsAsFactors = FALSE
    )

    estimand_label <- get_estimand_label(estimand)
    if(length(estimand_label) > 0) {
      ret <- cbind(ret, estimand_label=estimand_label, row.names=NULL, stringsAsFactors=FALSE)
    }
    ret

  }


  formals(f) <- formals(estimator_function)
  if(!"estimand" %in% names(formals(f))){formals(f)["estimand"] <- list(NULL)}
  if(!"label"%in% names(formals(f))){formals(f)$label <- alist(a=)$a}

  attributes(f) <- attributes(estimator_function)


  f
}



#' @param data a data.frame
#' @param model A model function, e.g. lm or glm. By default, the model is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param coefficients Symbols or literal character vector of coefficients that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept coefficient; if TRUE return all coefficients. To escape non-standard-evaluation use \code{!!}.
#' @rdname declare_estimator
model_handler <- function(data, ..., model = estimatr::difference_in_means, coefficients = FALSE) {

    coefficient_names <- enquo(coefficients) # forces evaluation of quosure
    coefficient_names <- reveal_nse_helper(coefficient_names)

    # estimator_function_internal <- function(data) {
    args <- quos(...)
    results <- eval_tidy(quo(model(!!!args, data=data)))

    results <- fit2tidy(results, coefficient_names)

    results
}

validation_fn(model_handler) <-  function(ret, dots, label){
  declare_time_error_if_data(ret)


  if("model" %in% names(dots)) {
    model <- eval_tidy(dots$model)
    if(!is.function(model) || ! "data" %in% names(formals(model))){
      declare_time_error("Must provide a function for `model` which takes a `data` argument.", ret)
    }
  }
  ret
}



#' @param estimand a declare_estimand step object, or a character label, or a list of either
#' @rdname declare_estimator
estimator_handler <- tidy_estimator(model_handler)

fit2tidy <- function(fit, coefficients = FALSE) {
  summ <- summary(fit)$coefficients
  summ <-
    summ[, tolower(substr(colnames(summ), 1, 3)) %in% c("est", "std", "pr("), drop = FALSE]
  ci <- suppressMessages(as.data.frame(confint(fit)))
  return_data <-
    data.frame(coefficient = rownames(summ),
               summ,
               ci,
               stringsAsFactors = FALSE, row.names = NULL)
  colnames(return_data) <- c("coefficient","est", "se", "p", "ci_lower", "ci_upper")


  if (is.character(coefficients)) {
    return_data <- return_data[return_data$coefficient %in% coefficients, ,drop = FALSE]
  } else if(is.logical(coefficients) && !coefficients) {
    return_data <- return_data[which.max(return_data$coefficient != "(Intercept)"), ,drop = FALSE]
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

