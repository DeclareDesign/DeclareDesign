#' Reveal Observed Outcomes
#' @param data A data.frame containing columns of potential outcomes and an assignment variable
#'
#' @param outcome_variable_names The outcome prefix(es) of the potential outcomes
#' @param assignment_variable_names The bare (unquote) name(s) of the assignment variable
#' @param attrition_variable_names The bare (unquote) name of the attrition variable
#'
#' @details
#'
#' Typically, a design includes a potential outcomes declaration and an assignment declaration. Reveal outcomes uses the random assignment to pluck out the correct potential outcomes. This is analogous to the "switching equation" (Gerber and Green 2012, Chapter 2).
#'
#'
#' @importFrom rlang enexpr lang_args expr_text
#'
#' @export
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_assignment,
#'                          reveal_outcomes)
#'
#' design
reveal_outcomes <-
  function(data = NULL, ...,
           outcome_variable_names = Y,
           assignment_variable_names = Z,
           attrition_variable_names = NULL) {

    if(!is.data.frame(data)) {
      stop("Please provide data to reveal_outcomes.")
    }

    # Setup to handle NSE
    outcome <- reveal_nse_helper(enquo(outcome_variable_names))
    attrition <- reveal_nse_helper(enquo(attrition_variable_names))
    assignment <- reveal_nse_helper(enquo(assignment_variable_names))

    assignment <- rep(assignment, length.out=max(length(outcome), length(attrition)))

    for (i in seq_along(outcome)) {
      data[, outcome[i]] <- switching_equation(data, outcome[i], assignment[i])
    }

    for (i in seq_along(attrition)) {
      response  <- switching_equation(data, attrition[i], assignment[i])
      data[response == 0, outcome[i]] <- NA
    }

    return(data)
}

attributes(reveal_outcomes) <- list(step_type = "reveal_outcomes", causal_type= "dgp")




switching_equation <- function(data, outcome, assignment) {

  R <- 1:nrow(data)

  potential_outcome_columns <- paste(outcome, assignment, data[[assignment]], sep = "_")

  data <- data[ , unique(potential_outcome_columns), drop=FALSE]

  C <- match(potential_outcome_columns, colnames(data))

  if(anyNA(C)) {
    stop(
      "You did not provide all the potential outcomes columns required to draw the outcome: ", outcome, ".\n",
      paste("  * ", unique(potential_outcome_columns[is.na(C)], collapse="\n"))
    )
  }

  data[cbind(R,C)]

}


reveal_nse_helper <- function(X) {
  if(is.character(X))     X
  else if(is.name(X))     as.character(X)
  else if(is_quosure(X))  reveal_nse_helper(quo_expr(X))
  else if(is.call(X))     unlist(lapply(X[-1], reveal_nse_helper))
}


#' Declare a Reveal Outcomes step
#'
#' @param ... arguments for the handler
#' @param handler a handler function
#' @param label a step label
#'
#' @export
declare_reveal <- make_declarations(reveal_outcomes, "reveal_outcomes")
