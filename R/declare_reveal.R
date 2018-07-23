#' Declare a reveal outcomes step
#'
#' Potential outcomes declarations indicate what outcomes would obtain for different possible values of assignment variables. 
#' To reveal actual outcomes we combine assignments with potential outcomes. \code{declare_reveal} provides information on how this
#' revelation should be implemented, identifying the relevant assignment variables (for example created by \code{declare_assignment}) and outcome variables. 
#' Revelation steps are usefully included after declaration of all assignments of conditions required to determine the realized outcome.
#' If a revelation is not declared DeclareDesign will try to guess appropriate revelations though explicit revelation is recommended.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
declare_reveal <- make_declarations(reveal_outcomes_handler, "reveal")

#' @param data A data.frame containing columns for assignment and potential outcomes.
#'
#' @param outcome_variables The outcome prefix(es) of the potential outcomes.
#' @param assignment_variables Unquoted name(s) of the assignment variable(s).
#' @param attrition_variables Unquoted name of the attrition variable.
#'
#' @details
#'
#' \code{declare_reveal} declares how outcomes should be realized.  
#' A "revelation" uses the random assignment to pluck out the correct potential outcomes (Gerber and Green 2012, Chapter 2).
#' If you create a simple design (with assignment variable Z and outcome variable Y) with the + operator but omit a reveal declaration, DeclareDesign will attempt to insert a revelation  step automatically.
#' If you have multiple outcomes to reveal or different names for the outcome or assignment variables, use \code{declare_reveal} to customize which outcomes are revealed.
#' Revelation requires that every named outcome variable is a function of every named assignment variable within a step. Thus if multiple outcome variables depend on different assignment variables, multiple revelations are needed.  
#'
#' 
#'
#' @importFrom rlang enexpr lang_args expr_text
#'
#' @export
#' @rdname declare_reveal
#'
#' @examples
#'
#' my_population <- declare_population(N = 100, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = noise, Y_Z_1 = noise +
#'   rnorm(N, mean = 2, sd = 2))
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' my_reveal <- declare_reveal()
#'
#' design <- my_population +
#'   my_potential_outcomes +
#'   my_assignment +
#'   my_reveal
#'
#' design
#'
#' #  Here the + operator results in the same design being
#' #  created, because it automatically adds a declare_reveal step.
#'
#' design <- my_population + my_potential_outcomes + my_assignment
#'
#' # Declaring multiple assignment variables or multiple outcome variables
#'
#' population   <- declare_population(N = 10)
#' potentials_1 <- declare_potential_outcomes(Y1 ~ Z)  
#' potentials_2 <- declare_potential_outcomes(Y2 ~ 1 + 2*Z)  
#' potentials_3 <- declare_potential_outcomes(Y3 ~ 1 - X*Z, conditions = list(X = 0:1, Z = 0:1))  
#' assignment_Z <- declare_assignment(assignment_variable = "Z")
#' assignment_X <- declare_assignment(assignment_variable = "X")
#' reveal_1     <- declare_reveal(outcome_variables = c("Y1", "Y2"), assignment_variables = "Z")
#' reveal_2     <- declare_reveal(outcome_variables = "Y3", assignment_variables = c("X", "Z"))
#'
#' # Note here that the reveal cannot be done in one step, e.g. by using
#' # declare_reveal(outcome_variables = c("Y1", "Y2", "Y3"),
#' #   assignment_variables = c("X","Z"))
#' # The reason is that in each revelation all outcome variables should be a
#' # function of all assignment variables.
#' 
#' # declare_reveal can also be used to declare outcomes that include attrition
#' 
#' population <- declare_population(N = 100, age = sample(18:95, N, replace = TRUE))
#' 
#' potential_outcomes_Y <- declare_potential_outcomes(Y ~ .25 * Z + .01 * age * Z)
#' 
#' assignment <- declare_assignment(m = 25)
#' 
#' potential_outcomes_attrition <- 
#'   declare_potential_outcomes(R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))
#' 
#' reveal_attrition <- declare_reveal(outcome_variables = "R")
#' reveal_outcomes <- declare_reveal(outcome_variables = "Y", attrition_variables = "R")
#' 
#' my_design <- population + potential_outcomes_Y + potential_outcomes_attrition + 
#'   my_assignment + reveal_attrition + reveal_outcomes
#'
reveal_outcomes_handler <- function(data = NULL,
                                    outcome_variables = Y,
                                    assignment_variables = Z,
                                    attrition_variables = NULL, ...) {
  if (!is.character(outcome_variables)) {
    stop("outcome_variables should already be converted to characters")
  }
  if (!is.character(assignment_variables)) {
    stop("assignment_variables should already be converted to characters")
  }
  if (!is.null(attrition_variables) &&
    !is.character(attrition_variables)) {
    stop("attrition_variables should already be converted to characters")
  }

  for (i in seq_along(outcome_variables)) {
    data[, outcome_variables[i]] <- switching_equation(
      data,
      outcome_variables[i],
      assignment_variables
    )
  }

  for (i in seq_along(attrition_variables)) {
    response <- switching_equation(
      data,
      attrition_variables[i],
      assignment_variables
    )
    data[response == 0, outcome_variables[i]] <- NA
  }

  data
}


validation_fn(reveal_outcomes_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  dots <- reveal_nse_helper_dots(dots, "outcome_variables", reveal_outcomes_handler)
  dots <- reveal_nse_helper_dots(dots, "assignment_variables", reveal_outcomes_handler)
  dots <- reveal_nse_helper_dots(dots, "attrition_variables", reveal_outcomes_handler)

  ret <- build_step(
    currydata(
      reveal_outcomes_handler,
      dots,
      strictDataParam = attr(ret, "strictDataParam")
    ),
    handler = reveal_outcomes_handler,
    dots = dots,
    label = label,
    step_type = attr(ret, "step_type"),
    causal_type = attr(ret, "causal_type"),
    call = attr(ret, "call")
  )

  structure(ret,
    step_meta = dots[c(
      "attrition_variable",
      "outcome_variables",
      "assignment_variables"
    )]
  )
}

switching_equation <- function(data, outcome, assignments) {
  potential_cols <- mapply(paste,
    assignments,
    data[, assignments, drop = FALSE],
    sep = "_",
    SIMPLIFY = FALSE
  )
  potential_cols <- do.call(paste, c(outcome, potential_cols, sep = "_"))

  upoc <- unique(potential_cols)

  if (!(all(upoc %in% colnames(data)))) {
    stop(
      "Must provide all potential outcomes columns referenced by the assignment variable (",
      assignments,
      ").\n",
      "`data` did not include:\n",
      paste("  * ", sort(setdiff(
        upoc, colnames(data)
      )), collapse = "\n")
    )
  }

  data <- data[, upoc, drop = FALSE]

  R <- seq_len(nrow(data))
  C <- match(potential_cols, colnames(data))

  data[cbind(R, C)]
}
