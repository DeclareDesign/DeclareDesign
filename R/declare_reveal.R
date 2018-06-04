

#' Declare a Reveal Outcomes step
#' 
#' Potential outcomes declarations indicating what outcomes exist as a function of assignment variables. But one more step is required, revealing outcomes after treatment is realized. \code{declare_reveal} declares the set of outcome variable names, for example created by \code{declare_potential_outcomes} and the set of assignment variables \code{declare_assignment}. 
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
declare_reveal <- make_declarations(reveal_outcomes_handler, "reveal_outcomes")

#' @param data A data.frame containing columns of potential outcomes and an assignment variable
#'
#' @param outcome_variables The outcome prefix(es) of the potential outcomes
#' @param assignment_variables The bare (unquote) name(s) of the assignment variable
#' @param attrition_variables The bare (unquote) name of the attrition variable
#'
#' @details
#'
#' \code{declare_reveal} declares the realization of outcomes. Typically, when you create a design with \code{declare_design}, the reveal outcomes step is automatically added. When you use the outcome `Y` and a single assignment variable named `Z`, \code{declare_design} adds a reveal outcomes step at the appropriate point. If you have multiple outcomes to reveal or different names for the outcome or assignment variables, use \code{declare_reveal} to customize which outcomes are revealed.
#'
#' Designs for causal inference includes a potential outcomes declaration and an assignment declaration. Reveal outcomes uses the random assignment to pluck out the correct potential outcomes. This is analogous to the "switching equation" (Gerber and Green 2012, Chapter 2).
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_assignment,
#'                          my_reveal)
#'
#' design
#'
#' # By default, declare_design, results in the same design being 
#' #  created, because it automatically adds a declare_reveal step.
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_assignment)
#'                          
#' # Declaring multiple assignment variables can be done easily with !!! syntax.
#' 
#' my_outcomes <- c("Y1", "Y2", "Y3")
#' 
#' my_reveal <- declare_reveal(outcome_variables = !!!my_outcomes)
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
    data[, outcome_variables[i]] <- switching_equation(data, 
                                                       outcome_variables[i], 
                                                       assignment_variables)
  }

  for (i in seq_along(attrition_variables)) {
    response  <- switching_equation(data, 
                                    attrition_variables[i], 
                                    assignment_variables)
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
            step_meta = dots[c("attrition_variable",
                               "outcome_variables",
                               "assignment_variables")])
}

switching_equation <- function(data, outcome, assignments) {
  potential_cols <- mapply(paste,
                           assignments,
                           data[, assignments, drop = FALSE],
                           sep = "_",
                           SIMPLIFY = FALSE)
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
  
  R <- 1:nrow(data)
  C <- match(potential_outcome_columns, colnames(data))
  
  data[cbind(R, C)]
  
}

###############################################################################
## Helper functions for declaratiosn that should work either with symbols,
## string literals, or functions of either
## eg Y:Z => c("Y","Z")

reveal_nse_helper <- function(X) {
  if (is.character(X) || is.logical(X))
    X
  else if (is.name(X))
    as.character(X)
  else if (is_quosure(X))
    reveal_nse_helper(quo_expr(X))
  else if (is.call(X))
    unlist(lapply(X[-1], reveal_nse_helper))
}

reveal_nse_helper_dots <- function(dots, what, handler) {
  if (what %in% names(dots)) {
    dots[[what]] <- reveal_nse_helper(dots[[what]])
  } else if (!is.null(formals(handler)[[what]])) {
    dots[[what]] <- as.character(formals(handler)[[what]])
  }
  
  dots
}
