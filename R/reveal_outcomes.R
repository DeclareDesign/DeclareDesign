
#' @export
reveal_outcomes <- function(data, outcome_variable_name = Y, assignment_variable_name = Z, attrition_variable_name = NULL){

  variable_names <- names(data)

  outcome_variable_name <- as.character(substitute(outcome_variable_name))
  assignment_variable_name <- as.character(substitute(assignment_variable_name))
  attrition_variable_name <- substitute(attrition_variable_name)
  if (!is.null(attrition_variable_name)) {
    attrition_variable_name <- as.character(attrition_variable_name)
  }

  for (outcome_variable in outcome_variable_name) {
    for (assignment_variable in assignment_variable_name) {

      potential_outcome_variable_names <- variable_names[startsWith(variable_names, paste0(outcome_variable, "_", assignment_variable, "_"))]

      condition_names_potential_outcome_columns <- sapply(potential_outcome_variable_names, FUN = function(x)
        substr(x, nchar(paste0(outcome_variable, "_", assignment_variable, "_")) + 1, nchar(x)))

      condition_names_assignment_variable <- unique(data[, assignment_variable])

      conditions_without_columns <- condition_names_assignment_variable[!(condition_names_assignment_variable %in% condition_names_potential_outcome_columns)]

      if (length(conditions_without_columns) > 0) {
        stop(paste0("The conditions in the assignment variable labeled ", paste0(conditions_without_columns, collapse = ", "),
                    " do not have corresponding potential outcome columns. They should be named ",
                    paste0(paste0(outcome_variable, "_", assignment_variable, "_", conditions_without_columns), collapse = " and "), ". "))
      }

      data[, outcome_variable] <- NA
      for (cond in condition_names_assignment_variable) {
        data[data[, assignment_variable] == cond, outcome_variable] <-
          data[data[, assignment_variable] == cond, paste0(outcome_variable, "_", assignment_variable, "_", cond)]
      }
    }

    if (!is.null(attrition_variable_name)) {
      data[data[, attrition_variable_name] == 0, outcome_variable] <- NA
    }
  }

  return(data)

}

attributes(reveal_outcomes) <- list(type = "reveal_outcomes")

