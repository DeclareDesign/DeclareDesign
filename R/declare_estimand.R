#' Declare Estimand
#'
#' @description Declares estimands which are the subjects of inquiry and can be estimated by an estimator
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @export
#'
#' @examples
#'
#' # First, set up the rest of a design for use below
#' set.seed(42)
#' my_population <- declare_population(N = 100, X = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   formula = Y ~ (.25 + X) * Z + rnorm(N),
#'   conditions = c(0, 1)
#' )
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' my_reveal <- declare_reveal()
#'
#' design_stub <- declare_design(
#'   my_population, my_potential_outcomes,
#'   my_assignment, my_reveal
#' )
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
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimand(dat)
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' design_def <- declare_design(
#'   my_population, my_potential_outcomes,
#'   my_estimand, my_assignment, my_reveal,
#'   my_estimator
#' )
#'
#' conduct_design(design_def)
#'
#' # ----------
#' # 2. Multiple estimands
#' # ----------
#'
#' # You can also specify multiple estimands at a time
#' ate <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#' att <- declare_estimand(`E[ATT]` = mean(c(Y_Z_1 - Y_Z_0)[Z == 1]))
#'
#' # With multiple estimands, you can use one estimator for both...
#' my_estimator_two <- declare_estimator(
#'   Y ~ Z,
#'   estimand = c(ate, att)
#' )
#'
#' design_two <- declare_design(
#'   my_population, my_potential_outcomes,
#'   my_assignment, my_reveal, ate, att,
#'   my_estimator_two
#' )
#'
#' conduct_design(design_two)
#'
#' # You can also use different coefficients from an estimator for
#' # two estimands, with a slightly different syntax
#'
#' # Name your estimands the coefficient name they get in your
#' # estimator, and set `coefficients = TRUE`
#' two_mands <- declare_estimand(
#'   `(Intercept)` = mean(Y_Z_0),
#'   `Z` = mean(Y_Z_1 - Y_Z_0),
#'   coefficients = TRUE
#' )
#'
#' # Specify your estimand as usual, but now set
#' # `coefficients = TRUE`
#' my_estimator_double <- declare_estimator(
#'   Y ~ Z,
#'   estimand = two_mands,
#'   model = lm,
#'   coefficients = TRUE
#' )
#'
#' design_double <- declare_design(
#'   my_population, my_potential_outcomes,
#'   my_assignment, my_reveal, two_mands,
#'   my_estimator_double
#' )
#'
#' conduct_design(design_double)
#'
#' # ----------
#' # 3. Custom estimands
#' # ----------
#'
#' my_estimand_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(estimand = setNames(ret, label))
#' }
#' my_estimand_custom <- declare_estimand(
#'   handler = my_estimand_function,
#'   label = "medianTE"
#' )
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
#' design_cust <- declare_design(
#'   my_population, my_potential_outcomes,
#'   my_assignment, my_reveal, my_estimand_custom,
#'   my_estimator_custom
#' )
#'
#' conduct_design(design_cust)
#'
declare_estimand <- make_declarations(estimand_handler, "estimand", causal_type="estimand", default_label="my_estimand")


#' @importFrom rlang eval_tidy quos  is_quosure
estimand_handler <- function(data, ..., subset = NULL, coefficients=FALSE, label) {
  options <- quos(...)
  if(names(options)[1] == "") names(options)[1] <- label

  subset <- substitute(subset)
  if(is_quosure(subset)) subset <- subset[[2]]
  idx <- eval_tidy(subset, data = data)
  if (!is.null(idx)) {
    data <- data[idx, , drop = FALSE]
  }

  ret <- vector("list", length(options))
  for(i in seq_along(options)){
    ret[i] <- eval_tidy(options[[i]], data=data)
  }
  ret <- simplify2array(ret)

  if(coefficients){
    data.frame(estimand_label=label,
               coefficient=names(options),
               estimand=ret,
               stringsAsFactors = FALSE)


  } else {
    data.frame(estimand_label=names(options),
               estimand=ret,
               stringsAsFactors = FALSE)
  }
}

validation_fn(estimand_handler) <-  function(ret, dots, label){
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)

  declare_time_error_if_data(ret)


  # Don't overwrite label-label with splat label if coefficient names are true
  if("coefficients" %in% dotnames && isTRUE(eval_tidy(dots$coefficients))) return(ret)

  maybeDotLabel <- dotnames[! dotnames %in% c("", names(formals(estimand_handler)) )]
  if(length(maybeDotLabel) == 1 ){
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }

  ret
}
