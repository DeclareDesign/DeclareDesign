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
#'  # condition_names defines the "range" of the potential outcomes function
#'  my_potential_outcomes <-
#'       declare_potential_outcomes(
#'       formula = Y ~ .25 * Z + .01 * age * Z,
#'       condition_names = 1:4)
#'
#' head(my_potential_outcomes(pop))
#'
declare_potential_outcomes <- make_declarations(potential_outcomes_function_default, "potential_outcomes");


#' @importFrom rlang quos quo lang_modify !!! eval_tidy is_formula quo_expr
potential_outcomes_function_default <-
  function(...,
           data,
           assignment_variable_name = "Z",
           condition_names = c(0, 1),
           level = NULL
           ) {
    options <- quos(...)

    level <- reveal_nse_helper(substitute(level))


    has_formula <-  is_formula(quo_expr(options[[1]]))

    if (has_formula) {
      # TODO: checks re: condition names and assignment variable names

      po_call <- quo(potential_outcomes_function_formula(
        data = !!data,
        formula=!!(options[[1]]),
        condition_names = !!condition_names,
        assignment_variable_name = !!assignment_variable_name,
        level = !!level
        ))
    } else {

      po_call <- quo(potential_outcomes_function_discrete(data=!!data, level=!!level, !!! options))
    }

    if (!is.null(level)) {
      po_call <- lang_modify(po_call, level = level)
    }

    eval_tidy(po_call)

  }

potential_outcomes_function_formula <-
  function(data,
           formula,
           condition_names,
           assignment_variable_name,
           level = NULL) {
    outcome_variable_name <- as.character(formula[[2]])

    # edit data to be at the level

    has_level <- !is.null(level)
    if (has_level) {

      # this code lifted from level() in fabricatr

      # get the set of variable names that are unique within the level you are adding vars to
      #  so the new vars can be a function of existing ones
      level_variables <-
        get_unique_variables_by_level(data = data, ID_label = level)

      formula_variables <- all.vars(f_rhs(formula))
      formula_variables <- formula_variables[!formula_variables %in% assignment_variable_name]
      formula_variables_not_in_level <-
        !formula_variables %in% level_variables
      if (any(formula_variables_not_in_level)) {
        stop(
            "You provided the variables ",
            paste(formula_variables[formula_variables_not_in_level], collapse = ", "),
            " to formula is not constant within level ",
            level,
            "."
        )
      }

      data_full <- data

      # construct a dataset with only those variables at this level
      data <-
        unique(data[, unique(c(level, level_variables)),
                    drop = FALSE])

    }

    for (cond in condition_names) {
      ## make a dataset that we manipulate to make PO columns
      ## by adding Z variables that are all 0's or all 1's for example
      data_environment <- list2env(data)
      data_environment$N <- nrow(data)

      assign(x = assignment_variable_name,
             value = rep(cond, nrow(data)),
             envir = data_environment)

      data[, paste0(c(outcome_variable_name, assignment_variable_name, cond),
                    collapse = "_")] <-
        eval(expr = formula[[3]], envir = data_environment)
    }

    # merge the data back

    if (has_level) {
      data <-
        merge(data_full[, setdiff(colnames(data_full), level_variables), drop = FALSE],
              data,
              by = level,
              all = TRUE,
              sort = FALSE)
    }

    return(data)
  }

#' @importFrom rlang quos quo lang_modify !!! eval_tidy !! :=
#' @importFrom fabricatr fabricate add_level modify_level
potential_outcomes_function_discrete <- function(data, level = NULL, ...) {
    options <- quos(...)

    if (!is.null(level)) {
      # if user sends a variable name in level, draw POs at the level
      #   defined by that variable. to do this, we send the options that
      #   were sent to fabricate to level first
      options <- quos(modify_level(!!!options))
      names(options) <- level
    }
    po_call <- quo(fabricate(data=!!data, !!!options))


    return(eval_tidy(po_call))

  }
