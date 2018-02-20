#' Potential Outcomes
#' @param ... Arguments to the potential_outcomes_function
#'
#' @param handler A function that accepts a data.frame as an argument and returns a data.frame with potential outcomes columns appended. See the examples for the behavior of the default function.
#' @param label A step label
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


potential_outcomes_handler <-  function(..., data) {
    # redispatch on formula
    potential_outcomes <- function(formula=NULL, ...) UseMethod("potential_outcomes", formula)
    potential_outcomes(..., data=data)
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
           data,
           conditions = c(0, 1),
           assignment_variable = "Z",
           level = NULL) {

    level <- reveal_nse_helper(enquo(level))

    outcome_variable <- as.character(formula[[2]])


    # Build a fabricate call -
    # fabricate( Z=1, Y_Z_1=f(Z), Z=2, Y_Z_2=f(Z), ..., Z=NULL)
    condition_quos <- quos()
    expr = formula[[3]]
    for(cond in conditions){
      out_name <- paste(outcome_variable, assignment_variable, cond, sep = "_")
      condition_quos <- c(condition_quos, quos(!!assignment_variable := !!cond, !!out_name := !!expr) )
    }
    condition_quos <- c(condition_quos, quos(!!assignment_variable := NULL))

    if(is.character(level)) {
      condition_quos <- quos(!!level := modify_level(!!!condition_quos))
    }


    structure(
      fabricate(data=data, !!!condition_quos),
      outcome_variable=outcome_variable,
      assignment_variable=assignment_variable)

}

#' @importFrom fabricatr fabricate add_level modify_level
#' @rdname declare_potential_outcomes
potential_outcomes.default <- function(formula=stop("Not provided"), ..., data, level = NULL) {
    level <- reveal_nse_helper(enquo(level))

    if (is.character(level)) {
      fabricate(data=data, modify_level(ID_label=!!level, ...))
    } else {
      fabricate(data=data, ...)
    }
}
