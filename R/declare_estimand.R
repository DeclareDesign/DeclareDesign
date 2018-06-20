#' Declare estimand
#'
#' @description Declares estimands which are the subjects of inquiry and can be estimated by an estimator
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @details
#'
#' For the default diagnosands, the return value of the handler should have `estimand_label` and `estimand` columns.
#'
#' @export
#'
#' @examples
#'
#' # Default handler
#'
#' my_estimand_ATE <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimand_ATT <- declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0), subset = Z == 1)
#'
#' # You can also use different coefficients from an model-based estimator for
#' # two estimands, with a slightly different syntax
#'
#' # Name your estimands the coefficient name they get in your
#' # estimator, and set `coefficients = TRUE`
#'
#' my_estimand_regression <- declare_estimand(
#'   `(Intercept)` = mean(Y_Z_0),
#'   `Z` = mean(Y_Z_1 - Y_Z_0),
#'   coefficients = TRUE,
#'   label="TrueRegressionParams"
#' )
#'
#' # Custom random assignment functions
#'
#' my_estimand_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(estimand_label = label,
#'              estimand = ret,
#'              time = Sys.time(),
#'              stringsAsFactors = FALSE)
#' }
#' my_estimand_custom <- declare_estimand(handler = my_estimand_function, label = "medianTE")
#'
#'
#' # Using with estimators
#'
#' # First, set up the rest of a design for use below
#' set.seed(42)
#'
#' pop <- declare_population(N = 100, X = rnorm(N))
#' pos <- declare_potential_outcomes(Y ~ (.25 + X) * Z + rnorm(N))
#' assgn <- declare_assignment(m = 50)
#' 
#' design_stub <- pop + 
#'   pos + 
#'   assgn + 
#'   declare_reveal()
#'
#' # Get example data to compute estimands on
#' dat <- draw_data(design_stub)
#'
#' # ----------
#' # 1. Single estimand
#' # ----------
#'
#' # Use the default estimand setup to
#' # declare an average treatment effect estimand
#'
#' my_estimand_ATE(dat)
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand_ATE, label = "estimator")
#'
#' design_def <- insert_step(design_stub, my_estimand_ATE, before = assgn)
#' design_def <- insert_step(design_def, my_estimator, after = "declare_reveal()")
#'
#' get_estimands(design_def)
#'
#' # ----------
#' # 2. Multiple estimands
#' # ----------
#'
#' # You can also specify multiple estimands at a time
#'
#' # With multiple estimands, you can use one estimator for both...
#' my_estimator_two <- declare_estimator(Y ~ Z, estimand = c(my_estimand_ATE, my_estimand_ATT))
#'
#' design_two <- insert_step(design_stub, my_estimand_ATE, before = assgn)
#' design_two <- insert_step(design_two, my_estimand_ATT, after = assgn)
#' design_two <- insert_step(design_two, my_estimator_two, after = "declare_reveal()")
#'
#' get_estimands(design_two)
#'
#'
#' # For the model based estimator, specify the estimand as usual,
#' # but also set `coefficients = TRUE`
#' my_estimator_double <- declare_estimator(
#'   Y ~ Z,
#'   estimand = my_estimand_regression,
#'   model = lm,
#'   coefficients = TRUE
#' )
#'
#' design_double <- insert_step(design_stub, my_estimand_regression, after = pos)
#' design_double <- insert_step(design_double, my_estimator_double, after = "declare_reveal()")
#'
#' run_design(design_double)
#'
#' # ----------
#' # 3. Custom estimands
#' # ----------
#'
#'
#' my_estimand_custom(dat)
#'
#' # Can also use custom estimator
#' my_estimator_function <- function(data){
#'   data.frame(est = with(data, median(Y)))
#' }
#' my_estimator_custom <-
#'   declare_estimator(handler = tidy_estimator(my_estimator_function),
#'                     estimand = my_estimand_custom)
#'
#' design_cust <- insert_step(design_stub, my_estimand_custom, before = assgn)
#' design_cust <- insert_step(design_cust, my_estimator_custom, after = "declare_reveal()")
#'
#' run_design(design_cust)
#' 
declare_estimand <- make_declarations(estimand_handler, "estimand", causal_type = "estimand", default_label = "my_estimand")

#' @param subset a subset expression
#' @param coefficients TRUE/FALSE
#' @param data a data.frame
#' @details
#'
#' If coefficients is TRUE, the names of ... will be returned in a `coefficients` column, and `estimand_label`
#' will contain the step label. This can be used as an additional dimension for use in diagnosis.
#'
#'
#' @importFrom rlang eval_tidy quos  is_quosure
#' @rdname declare_estimand
estimand_handler <- function(data, ..., subset = NULL, coefficients=FALSE, label) {
  options <- quos(...)
  if (names(options)[1] == "") names(options)[1] <- label

  subset <- substitute(subset)
  if (is_quosure(subset)) subset <- subset[[2]]
  idx <- eval_tidy(subset, data = data)
  if (!is.null(idx)) {
    data <- data[idx, , drop = FALSE]
  }

  ret <- vector("list", length(options))
  for (i in seq_along(options)) {
    ret[i] <- eval_tidy(options[[i]], data = data)
  }
  ret <- simplify2array(ret)

  if (coefficients) {
    data.frame(
      estimand_label = label,
      coefficient = names(options),
      estimand = ret,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      estimand_label = names(options),
      estimand = ret,
      stringsAsFactors = FALSE
    )
  }
}

validation_fn(estimand_handler) <-  function(ret, dots, label){
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)

  declare_time_error_if_data(ret)

  # Don't overwrite label-label with splat label if coefficient names are true
  if ("coefficients" %in% dotnames && isTRUE(eval_tidy(dots$coefficients))) return(ret)

  maybeDotLabel <- dotnames[!dotnames %in% c("", names(formals(estimand_handler)) )]
  if (any(duplicated(maybeDotLabel))) {
    stop(paste0("Please provide unique names for each estimand. Duplicates include ", 
                paste(maybeDotLabel[duplicated(maybeDotLabel)], collapse = ", "), "."), call. = FALSE)
  }
  if (length(maybeDotLabel) == 1) {
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }

  ret
}
