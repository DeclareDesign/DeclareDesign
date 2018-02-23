#' Potential Outcomes
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that returns a data.frame
#'
#' @export
#'
#'
#'
#' @details
#'
#' A `declare_potential_outcomes` declaration returns a function. That function takes data and returns data with potential outcomes columns appended. These columns describe the outcomes that each unit would express if that unit were in the corresponding treatment condition.
#'
#' The potential outcomes function can sometimes be a stumbling block for users, as some are uncomfortable asserting anything in particular about the very causal process that they are conducting a study to learn about! We recommend trying to imagine what your preferred theory would predict, what an alternative theory would predict, and what your study would reveal if there were no differences in potential outcomes for any unit (i.e., all treatment effects are zero).
#'
#'
#'
#'
#'
#' @examples
#' my_population <-
#' declare_population(N = 1000,
#'                    income = rnorm(N),
#'                    age = sample(18:95, N, replace = TRUE))
#' pop <- my_population()
#'
#' # By default, there are two ways of declaring potential outcomes:
#' # as separate variables or using a formula:
#'
#' # As separate variables
#'
#' my_potential_outcomes <-
#'      declare_potential_outcomes(
#'         Y_Z_0 = .05,
#'         Y_Z_1 = .30 + .01 * age)
#'
#' head(my_potential_outcomes(pop))
#'
#' # Using a formula
#'  my_potential_outcomes <- declare_potential_outcomes(
#'  formula = Y ~ .25 * Z + .01 * age * Z)
#'  pop_pos <- my_potential_outcomes(pop)
#'  head(pop_pos)
#'
#'  # conditions defines the "range" of the potential outcomes function
#'  my_potential_outcomes <-
#'       declare_potential_outcomes(
#'       formula = Y ~ .25 * Z + .01 * age * Z,
#'       conditions = 1:4)
#'
#' head(my_potential_outcomes(pop))
#'
declare_potential_outcomes <- make_declarations(potential_outcomes_handler, "potential_outcomes");

# level *must* be last argument, otherwise eager reveal_nse_helper in validation_fn will not work positionally
potential_outcomes_handler <-  function(..., data, level) {}

validation_fn(potential_outcomes_handler) <-  function(ret, dots, label) {
  if(getOption("debug.DeclareDesign.potential_outcome_validation", FALSE)) browser()


  # Below is a similar redispatch pattern for the validation_delegate at declare time
  validation_delegate <- function(formula=NULL, ...) {
    potential_outcomes <- function(formula, ...) UseMethod("potential_outcomes", formula)
    s3method <- getS3method("potential_outcomes", class(formula))
    if(getOption("debug.DeclareDesign.potential_outcome_validation_s3_lookup", FALSE)) browser()

    s3method
  }

  s3method <- eval_tidy(quo(validation_delegate(!!!dots)))

  # explicitly name all dots, for easier s3 handler validation
  dots <- rename_dots(s3method, dots)

  if("level" %in% names(dots)){
    dots$level <- reveal_nse_helper(dots$level)
  }


  ret <- build_step(currydata(s3method, dots, strictDataParam=attr(ret, "strictDataParam")),
                    handler=s3method,
                    dots=dots,
                    label=label,
                    step_type=attr(ret, "step_type"),
                    causal_type=attr(ret,"causal_type"),
                    call=attr(ret, "call"))

  if(has_validation_fn(s3method)) ret <- validate(s3method, ret, dots, label)


  ret
}

#' @param formula a formula to calculate Potential outcomes as functions of assignment variables
#' @param conditions vector specifying the values the assignment variable can realize
#' @param assignment_variable The name of the assignment variable
#' @param level a character specifying a level of hierarchy for fabricate to calculate at
#' @param data a data.frame
#' @importFrom fabricatr fabricate
#' @importFrom rlang quos := !! !!!
#' @rdname declare_potential_outcomes
potential_outcomes.formula <-
  function(formula,
           conditions = c(0, 1),
           assignment_variable = "Z",
           data,
           level = NULL) {

    outcome_variable <- as.character(formula[[2]])

    if(!is.list(conditions)){
      conditions <- setNames(list(conditions), assignment_variable)
    }


    conditions <- expand.grid(conditions, stringsAsFactors = FALSE)
    condition_names <- names(conditions)

    # Build a fabricate call -
    # fabricate( Z=1, Y_Z_1=f(Z), Z=2, Y_Z_2=f(Z), ..., Z=NULL)
    condition_quos <- quos()
    expr = formula[[3]]
    for(i in 1:nrow(conditions)){

      condition_values <- conditions[i,,drop=FALSE]
      out_name <- paste0(outcome_variable, "_", paste0(condition_names, "_", condition_values, collapse = "_"))

      condition_quos <- c(condition_quos, quos(!!!condition_values, !!out_name := !!expr) )
    }

    # clean up
    condition_values <-  lapply(condition_values, function(x) NULL)
    condition_quos <- c(condition_quos, quos(!!condition_values))

    if(is.character(level)) {
      condition_quos <- quos(!!level := modify_level(!!!condition_quos))
    }


    structure(
      fabricate(data=data, !!!condition_quos),
      outcome_variable=outcome_variable,
      assignment_variable=assignment_variable)

  }


validation_fn(potential_outcomes.formula) <- function(ret, dots, label){
  getFormulaOut <- function(...) formula
  formals(getFormulaOut) <- formals(potential_outcomes.formula)

  formula <- eval_tidy(quo(getFormulaOut(!!!dots)))

  if(length(formula) == 2){
    declare_time_error("Must provide an outcome  in potential outcomes formula", ret)
  }


  ret
}

#' @importFrom fabricatr fabricate add_level modify_level
#' @rdname declare_potential_outcomes
potential_outcomes.NULL <- function(formula=stop("Not provided"), ..., data, level = NULL) {

    if (is.character(level)) {
      fabricate(data=data, modify_level(ID_label=!!level, ...))
    } else {
      fabricate(data=data, ...)
    }
}
