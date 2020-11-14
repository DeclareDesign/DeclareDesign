#' Declare estimator
#'
#' @description Declares an estimator which generates estimates and associated statistics.
#' 
#' Use of \code{declare_test} is identical to use of \code{\link{declare_estimator}}. Use \code{declare_test} for hypothesis testing with no specific estimand in mind; use \code{declare_estimator} for hypothesis testing when you can link each estimate to an estimand. For example, \code{declare_test} could be used for a K-S test of distributional equality and \code{declare_estimator} for a difference-in-means estimate of an average treatment effect.
#'
#' @inheritParams declare_internal_inherit_params
#' 
#' @details
#' 
#' \code{declare_estimator} is designed to handle two main ways of generating parameter estimates from data.
#' 
#' In \code{declare_estimator}, you can optionally provide the name of an estimand or an objected created by \code{\link{declare_estimand}} to connect your estimate(s) to estimand(s).
#' 
#' The first is through \code{label_estimator(model_handler)}, which is the default value of the \code{handler} argument. Users can use standard modeling functions like lm, glm, or iv_robust. The models are summarized using the function passed to the \code{model_summary} argument. This will usually be a "tidier" like \code{broom::tidy}. The default \code{model_summary} function is \code{tidy_try}, which applies a tidy method if available, and if not, tries to make one on the fly.
#' 
#' An example of this approach is:
#' 
#' \code{declare_estimator(Y ~ Z + X, model = lm_robust, model_summary = tidy, term = "Z", estimand = "ATE")}
#' 
#' The second approach is using a custom data-in, data-out function, usually first passed to \code{label_estimator}. The reason to pass the custom function to \code{label_estimator} first is to enable clean labeling and linking to estimands.
#' 
#' An example of this approach is:
#' 
#' \code{
#' my_fun <- function(data){ with(data, median(Y[Z == 1]) - median(Y[Z == 0])) }
#' }
#' 
#' \code{
#' declare_estimator(handler = label_estimator(my_fun), estimand = "ATE")
#' }
#' 
#' @export
#' @importFrom estimatr difference_in_means
#'
#' @return A function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimator and associated statistics.
#'
#' @examples
#'
#' # Declare estimand
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#' 
#' # Declare estimator using the default handler using `difference_in_means`
#' # estimator from `estimatr` package. Returns the first non-intercept term
#' # as estimate
#' 
#' my_estimator_dim <- declare_estimator(Y ~ Z, estimand = "ATE", label = "DIM")
#'
#' # Use lm function from base R
#' my_estimator_lm <- declare_estimator(Y ~ Z, estimand = "ATE",
#'   model = lm, label = "LM")
#
#' # Use lm_robust (linear regression with robust standard errors) from
#' # `estimatr` package
#'
#' my_estimator_lm_rob <- declare_estimator(
#'   Y ~ Z,
#'   estimand = "ATE",
#'   model = lm_robust,
#'   label = "LM_Robust"
#' )
#'
#' # Set `term` if estimate of interest is not the first non-intercept variable
#' my_estimator_lm_rob_x <- declare_estimator(
#'   Y ~ X + Z,
#'   estimand = my_estimand,
#'   term = "Z",
#'   model = lm_robust
#' )
#'
#' # Use glm from base R
#' my_estimator_glm <- declare_estimator(
#'   Y ~ X + Z,
#'   family = "gaussian",
#'   estimand = my_estimand,
#'   term = "Z",
#'   model = glm
#' )
#'
#' # A probit
#' estimator_probit <- declare_estimator(
#'   Y ~ Z,
#'   model = glm,
#'   family = binomial(link = "probit"),
#'   term = "Z"
#' )
#'
#' # Declare estimator using a custom handler
#'
#' # Define your own estimator and use the `label_estimator` function for labeling
#' # Must have `data` argument that is a data.frame
#' my_estimator_function <- function(data){
#'   data.frame(estimate = with(data, mean(Y)))
#' }
#'
#' my_estimator_custom <- declare_estimator(
#'   handler = label_estimator(my_estimator_function),
#'   estimand = my_estimand
#' )
#'
#' # Customize labeling
#'
#' my_estimator_function <- function(data){
#'   data.frame(
#'     estimator_label = "foo",
#'     estimand_label = "bar",
#'     estimate = with(data, mean(Y)),
#'     n = nrow(data),
#'     stringsAsFactors = FALSE
#'   )
#' }
#'
#' my_estimator_custom2 <- declare_estimator(handler = my_estimator_function)
#'
#'
#' # Examples
#'
#' # First, set up the rest of a design
#' set.seed(42)
#'
#' design_def <-
#'   declare_population(N = 100, X = rnorm(N), W = rexp(N, 1), noise = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ .25 * Z + noise) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(m = 50) +
#'   reveal_outcomes() +
#'   my_estimator_dim
#'
#' draw_estimates(design_def)
#'
#' # Can also use declared estimator on a data.frame
#' dat <- draw_data(design_def)
#' my_estimator_dim(dat)
#'
#' # ----------
#' # 2. Using existing estimators
#' # ----------
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_lm_rob)
#' draw_estimates(design)
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_lm)
#' draw_estimates(design)
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_glm)
#' draw_estimates(design)
#'
#' # ----------
#' # 3. Using custom estimators
#' # ----------
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_custom)
#'
#' draw_estimates(design)
#'
#' # The names in your custom estimator return should match with
#' # your diagnosands when diagnosing a design
#' my_median <- function(data) data.frame(med = median(data$Y))
#'
#' my_estimator_median <- declare_estimator(
#'   handler = label_estimator(my_median),
#'   estimand = my_estimand
#' )
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_median)
#'
#' draw_estimates(design)
#'
#' my_diagnosand <- declare_diagnosands(med_to_estimand = mean(med - estimand))
#'
#' \dontrun{
#' diagnose_design(design, diagnosands = my_diagnosand, sims = 5,
#'   bootstrap_sims = FALSE)
#' }
#'
#' # ----------
#' # 4. Multiple estimators per estimand
#' # ----------
#'
#' design_two <- insert_step(design_def,  my_estimator_lm,
#'   after = my_estimator_dim)
#'
#' draw_estimates(design_two)
#'
#' \dontrun{
#' diagnose_design(design_two, sims = 5, bootstrap_sims = FALSE)
#' }
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
#' \code{label_estimator} takes a data-in-data out function to \code{fn}, and returns a data-in-data-out function that first runs the provided estimation function \code{fn} and then appends a label for the estimator and, if an estimand is provided, a label for the estimand.
#' 
#' @param fn A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval), and a term column for labeling coefficient estimates.
#' 
#' @rdname declare_estimator
#' @export
label_estimator <- function(fn) {
  if (!("data" %in% names(formals(fn)))) {
    stop("Must provide a `estimator_function` function with a data argument.")
  }

  f <- function(data, ..., estimand = NULL, label) {
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
      estimator_label = label,
      ret,
      stringsAsFactors = FALSE
    )

    estimand_label <- get_estimand_label(estimand)
    if (length(estimand_label) > 0) {
      ret <-
        cbind(
          ret,
          estimand_label = estimand_label,
          row.names = NULL,
          stringsAsFactors = FALSE
        )
    }
    ret
  }

  formals(f) <- formals(fn)
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
#' @param model A model function, e.g. lm or glm. By default, the model is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param model_summary A model-in data-out function to extract coefficient estimates or model summary statistics, such as \code{\link{tidy}} or \code{\link{glance}}. By default, the \code{DeclareDesign} model summary function \code{\link{tidy_try}} is used, which first attempts to use the available tidy method for the model object sent to \code{model}, then if not attempts to summarize coefficients using the \code{coef(summary())} and \code{confint} methods. If these do not exist for the model object, it fails.
#' @param term Symbols or literal character vector of term that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept term; if TRUE return all term. To escape non-standard-evaluation use \code{!!}.
#' @rdname declare_estimator 
#' @importFrom rlang eval_tidy
model_handler <-
  function(data,
             ...,
             model = estimatr::difference_in_means,
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

# helper methods for estimand = my_estimand arguments to estimator_handler
#
get_estimand_label <- function(estimand) {
  force(estimand) # no promise nonsense when we look at it
  switch(
    class(estimand)[1],
    "character" = estimand,
    "design_step" = attributes(estimand)$label,
    "list" = vapply(estimand, get_estimand_label, NA_character_),
    # note recursion here
    NULL = NULL,
    warning("Did not match class of `estimand`")
  )
}
