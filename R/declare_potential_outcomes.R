


#' Potential Outcomes
#' @param ... Arguments to the potential_outcomes_function
#'
#' @param potential_outcomes_function A function that accepts a data.frame as an argument and returns a data.frame with potential outcomes columns appended. See the examples for the behavior of the default function.
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
declare_potential_outcomes <-
  function(..., potential_outcomes_function = potential_outcomes_function_default) {
    args <- eval(substitute(alist(...)))
    env <- freeze_environment(parent.frame())
    func <- eval(potential_outcomes_function)

    if (!("data" %in% names(formals(func)))) {
      stop("Please provide a potential_outcomes_function with a data argument.")
    }

    potential_outcomes_function_internal <- function(data) {
      args$data <- data
      do.call(func, args = args, envir = env)
    }

    attributes(potential_outcomes_function_internal) <-
      list(call = match.call(), type = "potential_outcomes")

    return(potential_outcomes_function_internal)
  }

#' @importFrom rlang quos quo lang_modify !!! eval_tidy
#' @export
potential_outcomes_function_default <-
  function(data,
           assignment_variable_name = "Z",
           condition_names = c(0, 1),
           ...) {
    options <- quos(...)

    if ("formula" %in% names(options)) {
      # TODO: checks re: condition names and assignment variable names
      po_call <- quo(potential_outcomes_function_formula(!!! options))
      po_call <- lang_modify(po_call, data = data, assignment_variable_name = assignment_variable_name,
                             condition_names = condition_names)

      return(eval_tidy(po_call))

    } else{

      po_call <- quo(potential_outcomes_function_discrete(!!! options))
      po_call <- lang_modify(po_call, data = data)

      return(eval_tidy(po_call))
    }
  }

potential_outcomes_function_formula <-
  function(data,
           formula,
           condition_names,
           assignment_variable_name) {
    outcome_variable_name <- as.character(formula[[2]])

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

    return(data)
  }

#' @importFrom rlang quos quo lang_modify !!! eval_tidy
#' @importFrom fabricatr fabricate_data
potential_outcomes_function_discrete <- function(data, ...) {
  options <- quos(...)

  variable_names <- sapply(
    names(options),
    FUN =
      function(x)
        paste(strsplit(x, split = "_")[[1]][1], strsplit(x, split = "_")[[1]][2], sep = "_")
  )

  if (!all(variable_names == variable_names[1])) {
    stop("All of the variable names you create should begin with the same prefix, i.e. Y_Z_")
  }

  po_call <- quo(fabricate_data(!!! options))
  po_call <- lang_modify(po_call, data = data)

  return(eval_tidy(po_call))
}
