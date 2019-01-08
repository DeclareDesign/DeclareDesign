#' Declare estimand
#'
#' @description Declares estimands. Estimands are the subjects of inquiry and can be estimated by an estimator.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @details
#'
#' For the default diagnosands, the return value of the handler should have \code{estimand_label} and \code{estimand} columns.
#'
#' @export
#'
#' @examples
#'
#' # Set up a design stub for use in examples:
#'
#' my_population  <- declare_population(N = 100, X = rnorm(N))
#' my_potential_outcomes  <- declare_potential_outcomes(
#'   Y ~ (.25 + X) * Z + rnorm(N))
#' my_assignment  <- declare_assignment(m = 50)
#' design_stub <- my_population + my_potential_outcomes + my_assignment + 
#'   declare_reveal()
#'
#' # Get example data to compute estimands on
#' dat <- draw_data(design_stub)
#'
#' # ----------
#' # 1. Single estimand
#' # ----------
#'
#' # Declare an average treatment effect (ATE) estimand
#' 
#' my_estimand_ATE <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#' my_estimand_ATE(dat)
#' 
#' # or a conditional estimand
#' 
#' my_estimand_ATT <- declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0),
#'                                     subset = (Z == 1))
#' my_estimand_ATT (dat)
#' 
#' # Add estimands to a design along with estimators that reference them
#' 
#' my_estimator <- declare_estimator(Y ~ Z, 
#'   estimand = my_estimand_ATE, label = "estimator")
#'
#' design_one <- design_stub + my_estimand_ATE + my_estimator
#'
#' draw_estimands(design_one)
#'
#' # ----------
#' # 2. Multiple estimands
#' # ----------
#'
#' # You can also specify multiple estimands for a single estimator 
#'
#' # With multiple estimands, you can use one estimator for both...
#' 
#' my_estimator_two <- declare_estimator(Y ~ Z,
#'   estimand = c(my_estimand_ATE, my_estimand_ATT))
#'
#' design_two <- design_stub + my_estimand_ATE + 
#'   my_estimand_ATT + my_estimator_two
#' 
#' draw_estimands(design_two)
#'
#' # ----------
#' # 3. Paired estimands / estimators from a single model
#' # ----------
#'
#' # For convenience you can also declare multiple estimands
#' # simultaneously and connect these to the corresponding 
#' # terms for estimates used in the mode. 
#'
#' # Name your estimands the term name they get in your
#' # estimator, and set `term = TRUE`
#'
#' estimands_regression <- declare_estimand(
#'   `(Intercept)` = mean(Y_Z_0),
#'   `Z` = mean(Y_Z_1 - Y_Z_0),
#'   term = TRUE,
#'   label="Regression_Estimands"
#' )
#'
#' # For the model based estimator, specify the estimand as usual,
#' # but also set `term = TRUE`
#' estimators_regression <- declare_estimator(
#'   Y ~ Z,
#'   estimand = estimands_regression,
#'   model = lm,
#'   term = TRUE
#' )
#'
#' design_regression <- design_stub + estimands_regression +
#'   estimators_regression
#'
#' run_design(design_regression)
#'
#' # ----------
#' # 4. Custom estimand function
#' # ----------
#'
#' # You can declare more complex estimands by defining custom
#' # estimand functions:
#' 
#' estimand_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(estimand_label = label,
#'              estimand = ret,
#'              stringsAsFactors = FALSE)
#' }
#' 
#' estimand_custom <- declare_estimand(handler = estimand_function,
#'   label = "medianTE")
#'
#' estimand_custom(dat)
#'
#' # Use with custom estimators
#' estimator_function <- function(data){
#'   data.frame(estimate = with(data, median(Y)))
#' }
#' estimator_custom <-
#'   declare_estimator(handler = tidy_estimator(estimator_function),
#'                     estimand = estimand_custom)
#'
#' design_custom <- design_stub + estimand_custom +
#'   estimator_custom
#' 
#' run_design(design_custom)
#'
#' # ----------
#' # 5. Batch estimands and estimators
#' # ----------
#' 
#' # You can declare a group of estimands with distinct labels
#' # in one go and link them manually to a group of estimators.
#' # In this case you can add a \code{term} argument to the 
#' # custom estimators to identify them.
#' 
#' f1 <- function(data) {
#'   data.frame(estimand_label = c("control", "ate"),
#'              estimand = with(data, c(mean(Y_Z_0), mean(Y_Z_1 - Y_Z_0))),
#'              stringsAsFactors = FALSE)
#' }
#' estimands <- declare_estimand(handler = f1)
#' 
#' f2 <- function(data) data.frame(estimate = with(data,
#'                             c(mean(Y[Z == 0]),
#'                             mean(Y[Z == 1]) - mean(Y[Z == 0]))),
#'                             term = 1:2)
#' 
#' estimators <- declare_estimator(handler = tidy_estimator(f2), 
#'                             estimand = c("control", "ate"), label = "custom")
#'                             
#' design      <- design_stub + estimands + estimators
#' 
#' \dontrun{
#' diagnose_design(design, sims = 20, bootstrap_sims = FALSE,
#'                 diagnosands = declare_diagnosands(
#'                 select = c(mean_estimate, mean_estimand)))
#' }
declare_estimand <- make_declarations(estimand_handler, "estimand",
                                      causal_type = "estimand", 
                                      default_label = "estimand"
)

#' @rdname declare_estimand
#' @export
declare_estimands <- declare_estimand

#' @param subset a subset expression
#' @param term TRUE/FALSE
#' @param data a data.frame
#' @details
#'
#' If term is TRUE, the names of ... will be returned in a \code{term} column,
#' and \code{estimand_label} will contain the step label. This can be used as
#' an additional dimension for use in diagnosis.
#'
#'
#' @importFrom rlang eval_tidy quos  is_quosure
#' @rdname declare_estimand
estimand_handler <- function(data, ..., subset = NULL, term = FALSE, label) {
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
  
  if (term) {
    data.frame(
      estimand_label = label,
      term = names(options),
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

validation_fn(estimand_handler) <- function(ret, dots, label) {
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)
  
  declare_time_error_if_data(ret)
  
  # Don't overwrite label-label with splat label if term names are true
  if ("term" %in% dotnames && isTRUE(eval_tidy(dots$term))) {
    return(ret)
  }
  
  maybeDotLabel <- dotnames[!dotnames %in% c("", names(formals(estimand_handler)))]
  if (any(duplicated(maybeDotLabel))) {
    stop(paste0(
      "Please provide unique names for each estimand. Duplicates include ",
      paste(maybeDotLabel[duplicated(maybeDotLabel)], collapse = ", "), "."
    ), call. = FALSE)
  }
  if (length(maybeDotLabel) == 1) {
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }
  
  ret
}
