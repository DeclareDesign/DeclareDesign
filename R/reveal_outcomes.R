


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
  function(data = NULL,
           outcome_variable_names = Y,
           assignment_variable_names = Z,
           attrition_variable_name = NULL,
           outcome_function = NULL) {

    if (is.null(data)) {
      stop("Please provide data to reveal_outcomes.")
    }

    # Setup to handle NSE

    outcome_variable_names <- reveal_nse_helper(substitute(outcome_variable_names))

    assignment_variable_names <- reveal_nse_helper(substitute(assignment_variable_names))
    attrition_variable_name <- reveal_nse_helper(substitute(attrition_variable_name))



    for (outcome_variable_name in outcome_variable_names) {
      # Two Cases: Switching or Outcome Function

      if (is.null(outcome_function)) {
        data[, outcome_variable_name] <-
          switching_equation(
            data = data,
            outcome_variable_name = outcome_variable_name,
            assignment_variable_names = assignment_variable_names
          )

      } else {
        data <- outcome_function(data)
      }

      if (!is.null(attrition_variable_name)) {
        response_vec <-
          switching_equation(
            data = data,
            outcome_variable_name = attrition_variable_name,
            assignment_variable_names = assignment_variable_names
          )

        data[response_vec == 0, outcome_variable_name] <- NA

      }
    }

    return(data)

  }

attributes(reveal_outcomes) <- list(step_type = "reveal_outcomes", causal_type= "dgp")




switching_equation <- function(data,
                               outcome_variable_name,
                               assignment_variable_names) {

  data[,"_local_id"] <- 1:nrow(data)

  assignment_variable_df <-
    data[, assignment_variable_names, drop = FALSE]

  condition_combinations <-
    apply(
      X = assignment_variable_df,
      MARGIN = 1,
      FUN =  function(x) {
        paste0(paste0(assignment_variable_names, "_"), x, collapse = "_")
      }
    )

  potential_outcome_variable_names <-
    paste0(outcome_variable_name, "_", condition_combinations)

  if (!all(potential_outcome_variable_names %in% colnames(data))) {
    stop(paste0("You did not provide all the potential outcomes columns required to draw the outcome ", outcome_variable_name, "."))
  } else {
    data_list <- split(data, potential_outcome_variable_names)

    data_list <-
      mapply(
        FUN = function(df, cond) {
          df[, outcome_variable_name] <- df[, cond]
          return(df)
        },
        data_list,
        names(data_list),
        SIMPLIFY = FALSE
      )

    data <- do.call(rbind, data_list)
    data <- data[order(data$`_local_id`),]

    rownames(data) <- NULL

  }
  return(data[, outcome_variable_name, drop = TRUE])
}


reveal_nse_helper <- function(X) {
  if(is.character(X))     X
  else if(is.name(X))     as.character(X)
  else if(is_quosure(X))  reveal_nse_helper(quo_expr(X))
  else if(is.call(X))     unlist(lapply(X[-1], reveal_nse_helper))
}

#' @export
declare_reveal <- make_declarations(reveal_outcomes, "reveal_outcomes");
