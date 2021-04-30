#' Declare a reveal outcomes step
#'
#' Potential outcomes declarations indicate what outcomes would obtain for different possible values of assignment variables. 
#' But realized outcomes need to be "revealed." 
#' \code{declare_reveal} generates these realized outcomes using information on 
#' potential outcomes  (for instance generated via \code{declare_potential_outcomes})  and the relevant 
#' assignment variables (for example created by \code{declare_assignment}). 
#' Revelation steps are usefully included after declaration of all assignments of conditions required to determine the realized outcome.
#' If a revelation is not declared, DeclareDesign will try to guess appropriate revelations. Explicit revelation is recommended however.
#' 
#' This function was previously called \code{declare_reveal}. You can still use either one.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
declare_reveal <- make_declarations(declare_reveal_handler, "reveal")

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
#' Revelation requires that every named outcome variable is a function of every named assignment variable within a step. Thus if multiple outcome variables depend on different assignment variables, multiple revelations are needed.  
#'
#' 
#'
#' @importFrom rlang enexpr expr_text
#'
#' @rdname declare_reveal
#'
#' @examples
#'
#' design <- 
#'   declare_model(
#'     N = 100, 
#'     U = rnorm(N), 
#'     Y_Z_0 = U, 
#'     Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
#'   ) + 
#'   declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#'
#' # Declaring multiple assignment variables or multiple outcome variables
#'
#' design   <- 
#'   declare_model(
#'     N = 10,
#'     potential_outcomes(Y1 ~ Z),
#'     potential_outcomes(Y2 ~ 1 + 2 * Z),
#'     potential_outcomes(Y3 ~ 1 - X * Z, conditions = list(X = 0:1, Z = 0:1))
#'   ) + 
#'   declare_assignment(Z = complete_ra(N), legacy = FALSE) + 
#'   declare_assignment(X = complete_ra(N), legacy = FALSE) + 
#'   declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z), 
#'                       Y2 = reveal_outcomes(Y2 ~ Z),
#'                       Y3 = reveal_outcomes(Y3 ~ X + Z))
#' 
#' design <- 
#'   declare_model(
#'     N = 100, 
#'     age = sample(18:95, N, replace = TRUE),
#'     potential_outcomes(Y ~ .25 * Z + .01 * age * Z),
#'     potential_outcomes(R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))
#'   ) + 
#'   declare_assignment(Z = complete_ra(N, m = 25), legacy = FALSE)
#'   declare_measurement(R = reveal_outcomes(R ~ Z),
#'                       Y = reveal_outcomes(Y ~ Z),
#'                       Y = ifelse(R == 1, Y, NA))
declare_reveal_handler <- function(data = NULL,
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


validation_fn(declare_reveal_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  dots <- reveal_nse_helper_dots(dots, "outcome_variables", declare_reveal_handler)
  dots <- reveal_nse_helper_dots(dots, "assignment_variables", declare_reveal_handler)
  dots <- reveal_nse_helper_dots(dots, "attrition_variables", declare_reveal_handler)

  ret <- build_step(
    currydata(
      declare_reveal_handler,
      dots
    ),
    handler = declare_reveal_handler,
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

  as.data.frame(data)[cbind(R, C)]
}
