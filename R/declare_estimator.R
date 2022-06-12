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
#' The first is through \code{label_estimator(method_handler)}, which is the default value of the \code{handler} argument. Users can use standard method functions like lm, glm, or iv_robust. The methods are summarized using the function passed to the \code{summary} argument. This will usually be a "tidier" like \code{broom::tidy}. The default \code{summary} function is \code{tidy_try}, which applies a tidy method if available, and if not, tries to make one on the fly.
#' 
#' An example of this approach is:
#' 
#' \code{declare_estimator(Y ~ Z + X, .method = lm_robust, .summary = tidy, term = "Z", inquiry = "ATE")}
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
#'
#' # Setup for examples
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ rbinom(
#'       N, 1, prob = pnorm(0.2 * Z + 0.2 * gender + 0.1 * Z * gender + U)
#'     ))
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#' 
#' # default estimator is lm_robust with tidy summary
#' design_0 <-
#'   design +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' run_design(design_0)
#' 
#' # Linear regression using lm_robust and tidy summary
#' design_1 <-
#'   design +
#'   declare_estimator(
#'     formula = Y ~ Z,
#'     .method = lm_robust,
#'     .summary = tidy,
#'     term = "Z",
#'     inquiry = "ATE",
#'     label = "lm_no_controls"
#'   )
#' 
#' run_design(design_1)
#' 
#' # Use glance summary function to view model fit statistics
#' design_2 <-
#'   design +
#'   declare_estimator(.method = lm_robust,
#'                     formula = Y ~ Z,
#'                     .summary = glance)
#' 
#' run_design(design_2)
#' 
#' # Use declare_estimator to implement custom answer strategies
#' my_estimator <- function(data) {
#'   data.frame(estimate = mean(data$Y))
#' }
#' 
#' design_3 <-
#'   design +
#'   declare_inquiry(Y_bar = mean(Y)) +
#'   declare_estimator(handler = label_estimator(my_estimator),
#'                     label = "mean",
#'                     inquiry = "Y_bar")
#' 
#' run_design(design_3)
#' 
#' # Use `term` to select particular coefficients
#' design_4 <-
#'   design +
#'   declare_inquiry(difference_in_cates = mean(Y_Z_1[gender == 1] - Y_Z_0[gender == 1]) -
#'                     mean(Y_Z_1[gender == 0] - Y_Z_0[gender == 0])) +
#'   declare_estimator(Y ~ Z * gender,
#'                     term = "Z:gender",
#'                     inquiry = "difference_in_cates",
#'                     .method = lm_robust)
#' 
#' run_design(design_4)
#' 
#' # Use glm from base R
#' design_5 <-
#'   design +
#'   declare_estimator(Y ~ Z + gender,
#'                     family = "gaussian",
#'                     inquiry = "ATE",
#'                     .method = glm)
#' 
#' run_design(design_5)
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
#' design_6 <-
#'   design +
#'   declare_estimator(
#'     Y ~ Z + gender,
#'     .method = glm,
#'     family = binomial("logit"),
#'     .summary = tidy_margins,
#'     term = "Z"
#'   )
#' 
#' run_design(design_6)
#' 
#' # Multiple estimators for one inquiry
#' 
#' design_7 <-
#'   design +
#'   declare_estimator(Y ~ Z,
#'                     .method = lm_robust,
#'                     inquiry = "ATE",
#'                     label = "OLS") +
#'   declare_estimator(
#'     Y ~ Z + gender,
#'     .method = glm,
#'     family = binomial("logit"),
#'     .summary = tidy_margins,
#'     inquiry = "ATE",
#'     term = "Z",
#'     label = "logit"
#'   )
#' 
#' run_design(design_7)
#' 
#' 
#'
#'
#' 
declare_estimator <-
  make_declarations(
    label_estimator(method_handler),
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

  f <- function(data, ...,  inquiry = NULL, estimand = NULL, label) {
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
      
      if(exists("term") && is.character(term) && is.character(inquiry) && length(term) == length(inquiry)) {
        merge_df <- data.frame(term = term, inquiry = inquiry)
        ret <- merge(ret, merge_df, by = "term", all = TRUE, sort = FALSE)
      } else {
        ret <-
          cbind(
            ret,
            inquiry = inquiry,
            row.names = NULL,
            stringsAsFactors = FALSE
          )
      } 
      
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

#' @rdname DeclareDesign-deprecated
#' @section \code{tidy_estimator}:
#' For \code{tidy_estimator}, use \code{\link{label_estimator}}.
#'
#' @export
tidy_estimator <- function(estimator_function) {
  warning("tidy_estimator() has been deprecated. Please use label_estimator() instead.")
  label_estimator(fn = estimator_function)
}

#' @rdname DeclareDesign-deprecated
#' @section \code{model_handler}:
#' For \code{model_handler}, use \code{\link{method_handler}}.
#' 
#' @export
model_handler <- function(...) {
  warning("model_handler() has been deprecated. Please use method_handler() instead.")
  method_handler(...)
}

#' @param data a data.frame
#' @param .method A method function, e.g. lm or glm. By default, the method is the \code{\link{lm_robust}} function from the \link{estimatr} package, which fits OLS regression and calculates robust and cluster-robust standard errors.
#' @param .summary A method-in data-out function to extract coefficient estimates or method summary statistics, such as \code{\link{tidy}} or \code{\link{glance}}. By default, the \code{DeclareDesign} method summary function \code{\link{tidy_try}} is used, which first attempts to use the available tidy method for the method object sent to \code{method}, then if not attempts to summarize coefficients using the \code{coef(summary())} and \code{confint} methods. If these do not exist for the method object, it fails.
#' @param model Deprecated argument. Use \code{.method} instead.
#' @param model_summary Deprecated argument. Use \code{.summary} instead.
#' @param term Symbols or literal character vector of term that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept term; if TRUE return all term. To escape non-standard-evaluation use \code{!!}.
#' @rdname declare_estimator 
#' @importFrom rlang eval_tidy
method_handler <-
  function(data,
             ...,
             .method = estimatr::lm_robust,
             .summary = tidy_try,
             model,
             model_summary,
             term = FALSE) {
    
    if(!missing(model)) {
      .method <- model
      warning("The argument 'model = ' is deprecated. Please use '.method = ' instead.")
    }
    if(!missing(model_summary)) {
      .summary <- model_summary
      warning("The argument 'model_summary = ' is deprecated. Please use '.summary = ' instead.")
    }
    
    coefficient_names <-
      enquo(term) # forces evaluation of quosure
    coefficient_names <- reveal_nse_helper(coefficient_names)

    args <- quos(...)

    # todo special case weights offsets for glm etc?

    results <- eval_tidy(quo(.method(!!!args, data = data)))
    
    summary_fn <- interpret_summary(.summary)
    
    results <- eval_tidy(summary_fn(results))
    
    if("term" %in% colnames(results)) {
      if (is.character(coefficient_names)) {
        coefs_in_output <- coefficient_names %in% results$term
        if (!all(coefs_in_output)) {
          stop(
              "Not all of the terms declared in your estimator are present in the estimator's output, including ",
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

# this is an internal function that is a helper to allow users to provide the summary arg in a variety of formats
#' @importFrom rlang is_formula expr_interp as_function expr quo eval_bare is_character is_function
interpret_summary <- function(summary_fn) {
  # parts copied from dplyr:::as_inlined_function and dplyr:::as_fun_list
  
  if(is_formula(summary_fn)) {
    f <- expr_interp(summary_fn)
    # TODO: unsure of what env should be here!
    fn <- as_function(f, env = parent.frame())
    body(fn) <- expr({
      # pairlist(...)
      `_quo` <- quo(!!body(fn))
      eval_bare(`_quo`, parent.frame())
    })
    return(fn)
  } else if (is_character(summary_fn)) {
    return(get(summary_fn, envir = parent.frame(), mode = "function"))
  } else if (is_function(summary_fn)) {
    return(summary_fn)
  } else {
    stop("Please provide one sided formula, a function, or a function name to summary.")
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

validation_fn(method_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)
  
  if ("model" %in% names(dots)) {
    dots$.method <- dots$model
    dots$model <- NULL
  }
  
  if (".method" %in% names(dots)) {
    method <- eval_tidy(dots$.method)
    if (!is.function(method) || !"data" %in% names(formals(method))) {
      declare_time_error(
        "Must provide a function for `method` that takes a `data` argument.",
        ret
      )
    }
    
    attr(ret, "extra_summary") <-
      sprintf("Method:\t%s", as.character(f_rhs(dots$.method)))
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
