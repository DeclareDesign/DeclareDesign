#' Declare estimator
#'
#' @description Declares an estimator which generates estimates and associated statistics.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
#' @importFrom estimatr difference_in_means
#'
#' @return A function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimator and associated statistics.
#'
#' @section Custom Estimators:
#'
#' \code{estimator_functions} implementations should be tidy (accept and return a data.frame)
#'
#' \code{model} implementations should at a minimum provide S3 methods for \code{summary} and \code{confint}.
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
#' # Define your own estimator and use the `tidy_estimator` function for labeling
#' # Must have `data` argument that is a data.frame
#' my_estimator_function <- function(data){
#'   data.frame(estimate = with(data, mean(Y)))
#' }
#'
#' my_estimator_custom <- declare_estimator(
#'   handler = tidy_estimator(my_estimator_function),
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
#'   declare_reveal() +
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
#'   handler = tidy_estimator(my_median),
#'   estimand = my_estimand
#' )
#'
#' design <- replace_step(design_def, my_estimator_dim, my_estimator_median)
#'
#' draw_estimates(design)
#'
#' my_diagnosand <- declare_diagnosands(med_to_estimand = mean(med - estimand),
#'   keep_defaults = FALSE)
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
    estimator_handler,
    step_type = "estimator",
    causal_type = "estimator",
    default_label = "estimator"
  )

#' @rdname declare_estimator
#' @export
declare_estimators <- declare_estimator

#' @details
#' \code{tidy_estimator} takes an untidy estimation function, and returns a tidy handler which accepts standard labeling options.
#'
#' The intent here is to factor out the estimator/estimand labeling so that it can be reused by other model handlers.
#'
#' @param estimator_function A function that takes a data.frame as an argument and returns a data.frame with the estimates, summary statistics (i.e., standard error, p-value, and confidence interval) and a label.
#' @rdname declare_estimator
#' @export
tidy_estimator <- function(estimator_function) {
  if (!("data" %in% names(formals(estimator_function)))) {
    stop("Must provide a `estimator_function` function with a data argument.")
  }

  f <- function(data, ..., estimand = NULL, label) {
    calling_args <-
      names(match.call(expand.dots = FALSE)) %i% names(formals(estimator_function))

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

    ret <- eval_tidy(quo(estimator_function(data, !!!dots)))

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

  formals(f) <- formals(estimator_function)
  if (!"estimand" %in% names(formals(f))) {
    formals(f)["estimand"] <- list(NULL)
  }
  if (!"label" %in% names(formals(f))) {
    formals(f)$label <- alist(a = )$a
  }

  attributes(f) <- attributes(estimator_function)

  f
}

#' @param data a data.frame
#' @param model A model function, e.g. lm or glm. By default, the model is the \code{\link{difference_in_means}} function from the \link{estimatr} package.
#' @param term Symbols or literal character vector of term that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept term; if TRUE return all term. To escape non-standard-evaluation use \code{!!}.
#' @rdname declare_estimator
model_handler <-
  function(data,
             ...,
             model = estimatr::difference_in_means,
             term = FALSE) {
    coefficient_names <-
      enquo(term) # forces evaluation of quosure
    coefficient_names <- reveal_nse_helper(coefficient_names)

    args <- quos(...)

    # todo special case weights offsets for glm etc?

    results <- eval_tidy(quo(model(!!!args, data = data)))

    results <- fit2tidy(results, coefficient_names)

    results
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

#' @param estimand a declare_estimand step object, or a character label, or a list of either
#' @rdname declare_estimator
estimator_handler <- tidy_estimator(model_handler)

#' @importFrom generics tidy
#' @export
generics::tidy

tidy_default <- function(x, conf.int = TRUE) {
  # TODO: error checking -- are column names named as we expect
  
  val <- try({
    summ <- coef(summary(x))
    
    if(conf.int == TRUE) {
      ci <- suppressMessages(as.data.frame(confint(x)))
      tidy_df <-
        data.frame(
          term = rownames(summ),
          summ,
          ci,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      colnames(tidy_df) <-
        c(
          "term",
          "estimate",
          "std.error",
          "statistic",
          "p.value",
          "conf.low",
          "conf.high"
        )
    } else {
      tidy_df <-
        data.frame(
          term = rownames(summ),
          summ,
          ci,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      colnames(tidy_df) <-
        c(
          "term",
          "estimate",
          "std.error",
          "statistic",
          "p.value"
        )
    }
    
  }, silent = TRUE)
  
  if(class(val) == "try-error"){
    stop("The default tidy method for the model fit of class ", class(x), " failed. You may try installing and loading the broom package, or you can write your own tidy.", class(x), " method.", call. = FALSE)
  }
    
  tidy_df
}

#' @importFrom utils getS3method
hasS3Method <- function(f, obj) {
  for(i in class(obj)) {
    get_function <- try(getS3method(f, i), silent = TRUE)
    if(class(get_function) != "try-error" && is.function(get_function)) return(TRUE)
  }
  FALSE
}

# called by model_handler, resets columns names !!!
fit2tidy <- function(fit, term = FALSE) {
  
  # browser()
  if (hasS3Method("tidy", fit)) {
    tidy_df <- tidy(fit, conf.int = TRUE)
  } else {
    tidy_df <- try(tidy_default(fit, conf.int = TRUE), silent = TRUE)
    
    if(class(tidy_df) == "try-error"){
      stop("We were unable to tidy the output of the function provided to 'model'. 
           It is possible that the broom package has a tidier for that object type. 
           If not, you can use a custom estimator to 'estimator_function'.
           See examples in ?declare_estimator")
    }
  }
    
  if (is.character(term)) {
    coefs_in_output <- term %in% tidy_df$term
    if (!all(coefs_in_output)) {
      stop(
        "Not all of the terms declared in your estimator are present in the model output, including ",
        paste(term[!coefs_in_output], collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    tidy_df <- tidy_df[tidy_df$term %in% term, , drop = FALSE]
  } else if (is.logical(term) && !term) {
    tidy_df <-
      tidy_df[which.max(tidy_df$term != "(Intercept)"), , drop = FALSE]
  }

  tidy_df
}

# helper methods for estimand=my_estimand arguments to estimator_handler
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
