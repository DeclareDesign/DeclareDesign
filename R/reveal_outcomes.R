


#' Reveal Observed Outcomes
#' @param data A data.frame containing columns of potential outcomes and an assignment variable
#'
#' @param outcome_variable_names The outcome prefix(es) of the potential outcomes
#' @param assignment_variable_names The bare (unquote) name(s) of the assignment variable
#' @param attrition_variable_name The bare (unquote) name of the attrition variable
#' @param outcome_function If specified, \code{reveal_outcomes} draws outcomes using \code{outcome_function} rather than the switching equation.
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
           attrition_variable_name = NULL,
           outcome_function = NULL) {

    if(!is.data.frame(data)) {
      stop("Please provide data to reveal_outcomes.")
    }

    if(is.null(outcome_function)){
      outcome_function <- function(data, ...) {
        data[, outcome_variable_name___] <-
          switching_equation(
            data = data,
            outcome_variable_name = outcome_variable_name___, #Awful name because some of our tests fail to pluralize the argument, whacks this.
            assignment_variable_names = assignment_variable_names
          )
        data
      }
    }

    # Setup to handle NSE

    outcome_variable_names <- reveal_nse_helper(enquo(outcome_variable_names))

    assignment_variable_names <- reveal_nse_helper(enquo(assignment_variable_names))
    attrition_variable_name <- reveal_nse_helper(enquo(attrition_variable_name))


    for (outcome_variable_name___ in outcome_variable_names) {
      data <- outcome_function(data, ...)

      if (is.character(attrition_variable_name)) {
        response_vec <-
          switching_equation(
            data = data,
            outcome_variable_name = attrition_variable_name,
            assignment_variable_names = assignment_variable_names
          )

        data[response_vec == 0, outcome_variable_name___] <- NA

      }
    }

    return(data)

  }

attributes(reveal_outcomes) <- list(step_type = "reveal_outcomes", causal_type= "dgp")




switching_equation <- function(data,
                               outcome_variable_name,
                               assignment_variable_names) {

  R <- 1:nrow(data)

  potential_outcome_columns <- paste(outcome_variable_name, assignment_variable_names, data[[assignment_variable_names]], sep = "_")

  data <- data[ , unique(potential_outcome_columns), drop=FALSE]

  C <- match(potential_outcome_columns, colnames(data))

  if(anyNA(C)) {
    stop("You did not provide all the potential outcomes columns required to draw the outcome: ", outcome_variable_name, ".\n",
         paste("  * ", unique(potential_outcome_columns[is.na(C)], collapse="\n"))
         )
  }

  data[cbind(R,C)]

#
#   assignment_variable_df <-
#     data[, assignment_variable_names, drop = FALSE]
#
#   condition_combinations <-
#     apply(
#       X = assignment_variable_df,
#       MARGIN = 1,
#       FUN =  function(x) {
#         paste(assignment_variable_names,  x, sep = "_")
#       }
#     )
#
#   potential_outcome_variable_names <- paste(outcome_variable_name, condition_combinations, sep = "_")
#
#   if (!all(potential_outcome_variable_names %in% colnames(data))) {
#     stop("You did not provide all the potential outcomes columns required to draw the outcome ", outcome_variable_name, ".")
#   }
#
#   data_list <- split(data, potential_outcome_variable_names)
#
#   data_list <-
#     mapply(
#       FUN = function(df, cond) {
#         df[, outcome_variable_name] <- df[, cond]
#         return(df)
#       },
#       data_list,
#       names(data_list),
#       SIMPLIFY = FALSE
#     )
#
#   data <- do.call(rbind.data.frame, c(data_list, make.row.names=FALSE))
#
#   data[order(data$`_local_id`), outcome_variable_name, drop=TRUE]

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
