#' Declare estimator
#'
#' @description Declares an estimator which generates estimates and associated statistics.
#' 
#' Use of \code{declare_test} is identical to use of \code{\link{declare_estimator}}. Use \code{declare_test} for hypothesis testing with no specific inquiry in mind; use \code{declare_estimator} for hypothesis testing when you can link each estimate to an inquiry. For example, \code{declare_test} could be used for a K-S test of distributional equality and \code{declare_estimator} for a difference-in-means estimate of an average treatment effect.
#'
#' @inheritParams declare_internal_inherit_params
#' 
#' @details
#' 
#' \code{declare_estimator} is designed to handle two main ways of generating parameter estimates from data.
#' 
#' In \code{declare_estimator}, you can optionally provide the name of an inquiry or an objected created by \code{\link{declare_inquiry}} to connect your estimate(s) to inquiry(s).
#' 
#' The first is through \code{label_estimator(model_handler)}, which is the default value of the \code{handler} argument. Users can use standard modeling functions like lm, glm, or iv_robust. The models are summarized using the function passed to the \code{model_summary} argument. This will usually be a "tidier" like \code{broom::tidy}. The default \code{model_summary} function is \code{tidy_try}, which applies a tidy method if available, and if not, tries to make one on the fly.
#' 
#' An example of this approach is:
#' 
#' \code{declare_estimator(Y ~ Z + X, model = lm_robust, model_summary = tidy, term = "Z", inquiry = "ATE")}
#' 
#' The second approach is using a custom data-in, data-out function, usually first passed to \code{label_estimator}. The reason to pass the custom function to \code{label_estimator} first is to enable clean labeling and linking to inquiries.
#' 
#' An example of this approach is:
#' 
#' \code{
#' my_fun <- function(data){ with(data, median(Y[Z == 1]) - median(Y[Z == 0])) }
#' }
#' 
#' \code{
#' declare_estimator(handler = label_estimator(my_fun), inquiry = "ATE")
#' }
#' 
#' @export
#' @importFrom estimatr difference_in_means
#'
#' @return A function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimator and associated statistics.
#'
#' @examples
#' # base design
#' design <-
#'   declare_model(
#'     N = 100,
#'     female = rbinom(N, 1, 0.5),
#'     U = rnorm(N),
#'     potential_outcomes(
#'      Y ~ rbinom(N, 1, prob = pnorm(0.2 * Z + 0.2 * female + 0.1 * Z * female + U)))
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
#'   declare_assignment(Z = complete_ra(N, m = 50)) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#' 
#' # Most estimators are modeling functions like lm or glm.
#'   
#' # Default statistical model is estimatr::difference_in_means
#' design + declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # lm from base R (classical standard errors assuming homoskedasticity)
#' design + declare_estimator(Y ~ Z, model = lm, inquiry = "ATE")
#' 
#' # Use lm_robust (linear regression with heteroskedasticity-robust standard errors) 
#' # from `estimatr` package
#' 
#' design + declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")
#' 
#' # use `term` to select particular coefficients
#' design + declare_estimator(Y ~ Z*female, term = "Z:female", model = lm_robust)
#' 
#' # Use glm from base R
#' design + declare_estimator(
#'   Y ~ Z + female,
#'   family = "gaussian",
#'   inquiry = "ATE",
#'   model = glm
#' )
#' 
#' # If we use logit, we'll need to estimate the average marginal effect with 
#' # margins::margins. We wrap this up in function we'll pass to model_summary
#' 
#' library(margins) # for margins
#' library(broom) # for tidy
#' 
#' tidy_margins <- function(x) {
#'   tidy(margins(x, data = x$data), conf.int = TRUE)
#' }
#' 
#' design +
#'   declare_estimator(
#'     Y ~ Z + female,
#'     model = glm,
#'     family = binomial("logit"),
#'     model_summary = tidy_margins,
#'     term = "Z"
#'   ) 
#' 
#' # Multiple estimators for one inquiry
#' 
#' two_estimators <-
#'   design +
#'   declare_estimator(Y ~ Z,
#'                     model = lm_robust,
#'                     inquiry = "ATE",
#'                     label = "OLS") +
#'   declare_estimator(
#'     Y ~ Z + female,
#'     model = glm,
#'     family = binomial("logit"),
#'     model_summary = tidy_margins,
#'     inquiry = "ATE",
#'     term = "Z",
#'     label = "logit"
#'   )
#' 
#' run_design(two_estimators)
#' 
#' # Declare estimator using a custom handler
#' 
#' # Define your own estimator and use the `label_estimator` function for labeling
#' # Must have `data` argument that is a data.frame
#' my_dim_function <- function(data){
#'   data.frame(estimate = with(data, mean(Y[Z == 1]) - mean(Y[Z == 0])))
#' }
#' 
#' design + declare_estimator(
#'   handler = label_estimator(my_dim_function),
#'   inquiry = "ATE"
#' )
#' 
declare_estimator <-
  make_declarations(
    label_estimator(model_handler),
    step_type = "estimator",
    causal_type = "estimator",
    default_label = "estimator"
  )

#' @rdname declare_estimator
#' @export
declare_estimators <- declare_estimator

#' @details
#' \code{label_estimator} takes a data-in-data out function to \code{fn}, and returns a data-in-data-out function that first runs the provided estimation function \code{fn} and then appends a label for the estimator and, if an inquiry is provided, a label for the inquiry.
#' 
#' @param fn A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval), and a term column for labeling coefficient estimates.
#' 
#' @rdname declare_estimator
#' @export
label_estimator <- function(fn) {
  if (!("data" %in% names(formals(fn)))) {
    stop("Must provide a `estimator_function` function with a data argument.")
  }

  f <- function(data, ...,  inquiry = NULL,estimand = NULL, label) {
    calling_args <-
      names(match.call(expand.dots = FALSE)) %i% names(formals(fn))

    dots <- if ("..." %in% calling_args) {
      quos(...)
    } else {
      list()
    }

    calling_args <- setdiff(calling_args, c("", "data", "..."))

    for (e in calling_args) {
      dots[[e]] <-
        do.call(enquo, list(as.symbol(e))) # this *should* retrieve term names as quosure. IDK
    }

    ret <- eval_tidy(quo(fn(data, !!!dots)))

    ret <- data.frame(
      estimator = label,
      ret,
      stringsAsFactors = FALSE
    )


    if(!is.null(estimand) && !is.null(inquiry)) {stop("Please provide either an inquiry or an estimand, but not both")}
    if(!is.null(estimand)){
      inquiry <- estimand
      warning("The argument 'estimand = ' is deprecated. Please use 'inquiry = ' instead.", call. = FALSE)
    }
    
    
    inquiry <- get_inquiry(inquiry)
    if (length(inquiry) > 0) {
      ret <-
        cbind(
          ret,
          inquiry = inquiry,
          row.names = NULL,
          stringsAsFactors = FALSE
        )
    }
    ret
  }

  formals(f) <- formals(fn)
  if (!"inquiry" %in% names(formals(f))) {
    formals(f)["inquiry"] <- list(NULL)
  }
  if (!"estimand" %in% names(formals(f))) {
    formals(f)["estimand"] <- list(NULL)
  }
  
  if (!"label" %in% names(formals(f))) {
    formals(f)$label <- alist(a = )$a
  }

  attributes(f) <- attributes(fn)

  f
}

#' @rdname ourPkg-deprecated
#' @section \code{tidy_estimator}:
#' For \code{tidy_estimator}, use \code{\link{label_estimator}}.
#'
#' @export
tidy_estimator <- function(estimator_function) {
  warning("tidy_estimator() has been deprecated. Please use label_estimator() instead.")
  label_estimator(fn = estimator_function)
}

#' @param data a data.frame
#' @param model A model function, e.g. lm or glm. By default, the model is the \code{\link{lm_robust}} function from the \link{estimatr} package, which fits OLS regression and calculates robust and cluster-robust standard errors.
#' @param model_summary A model-in data-out function to extract coefficient estimates or model summary statistics, such as \code{\link{tidy}} or \code{\link{glance}}. By default, the \code{DeclareDesign} model summary function \code{\link{tidy_try}} is used, which first attempts to use the available tidy method for the model object sent to \code{model}, then if not attempts to summarize coefficients using the \code{coef(summary())} and \code{confint} methods. If these do not exist for the model object, it fails.
#' @param term Symbols or literal character vector of term that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept term; if TRUE return all term. To escape non-standard-evaluation use \code{!!}.
#' @rdname declare_estimator 
#' @importFrom rlang eval_tidy
model_handler <-
  function(data,
             ...,
             model = estimatr::lm_robust,
             model_summary = tidy_try,
             term = FALSE) {
    coefficient_names <-
      enquo(term) # forces evaluation of quosure
    coefficient_names <- reveal_nse_helper(coefficient_names)

    args <- quos(...)

    # todo special case weights offsets for glm etc?

    results <- eval_tidy(quo(model(!!!args, data = data)))
    
    model_summary_fn <- interpret_model_summary(model_summary)
    
    results <- eval_tidy(model_summary_fn(results))
    
    if("term" %in% colnames(results)) {
      if (is.character(coefficient_names)) {
        coefs_in_output <- coefficient_names %in% results$term
        if (!all(coefs_in_output)) {
          stop(
            "Not all of the terms declared in your estimator are present in the model output, including ",
            paste(coefficient_names[!coefs_in_output], collapse = ", "),
            ".",
            call. = FALSE
          )
        }
        results <- results[results$term %in% coefficient_names, , drop = FALSE]
      } else if (is.logical(coefficient_names) && !coefficient_names) {
        results <- results[which.max(results$term != "(Intercept)"), , drop = FALSE]
      }
    }
    
    results
    
  }

# this is an internal function that is a helper to allow users to provide the model_summary arg in a variety of formats
#' @importFrom rlang is_formula expr_interp as_function expr quo eval_bare is_character is_function
interpret_model_summary <- function(model_summary) {
  # parts copied from dplyr:::as_inlined_function and dplyr:::as_fun_list
  
  if(is_formula(model_summary)) {
    f <- expr_interp(model_summary)
    # TODO: unsure of what env should be here!
    fn <- as_function(f, env = parent.frame())
    body(fn) <- expr({
      # pairlist(...)
      `_quo` <- quo(!!body(fn))
      eval_bare(`_quo`, parent.frame())
    })
    return(fn)
  } else if (is_character(model_summary)) {
    return(get(model_summary, envir = parent.frame(), mode = "function"))
  } else if (is_function(model_summary)) {
    return(model_summary)
  } else {
    stop("Please provide one sided formula, a function, or a function name to model_summary.")
  }
}

validation_fn(model_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  if ("model" %in% names(dots)) {
    model <- eval_tidy(dots$model)
    if (!is.function(model) || !"data" %in% names(formals(model))) {
      declare_time_error(
        "Must provide a function for `model` that takes a `data` argument.",
        ret
      )
    }

    attr(ret, "extra_summary") <-
      sprintf("Model:\t%s", as.character(f_rhs(dots$model)))
  }
  ret
}

# helper methods for inquiry = my_inquiry arguments to estimator_handler
#
get_inquiry <- function(inquiry) {
  force(inquiry) # no promise nonsense when we look at it
  switch(
    class(inquiry)[1],
    "character" = inquiry,
    "design_step" = attributes(inquiry)$label,
    "list" = vapply(inquiry, get_inquiry, NA_character_),
    # note recursion here
    NULL = NULL,
    warning("Did not match class of `inquiry`")
  )
}
