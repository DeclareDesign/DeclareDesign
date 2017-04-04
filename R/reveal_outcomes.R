
#' @export
reveal_outcomes <- function(data, outcome_variable_name = "Y", assignment_variable_name = "Z"){

    variable_names <- names(data)
    potential_outcome_variable_names <- variable_names[startsWith(variable_names, paste0(outcome_variable_name, "_", assignment_variable_name, "_"))]

    condition_names_potential_outcome_columns <- sapply(potential_outcome_variable_names, FUN = function(x)
      substr(x, nchar(paste0(outcome_variable_name, "_", assignment_variable_name, "_")) + 1, nchar(x)))

    condition_names_assignment_variable <- unique(data[, assignment_variable_name])

    conditions_without_columns <- condition_names_assignment_variable[!(condition_names_assignment_variable %in% condition_names_potential_outcome_columns)]

    if(length(conditions_without_columns) > 0){
      stop(paste0("The conditions in the assignment variable labeled ", paste0(conditions_without_columns, collapse = ", "),
                  " do not have corresponding potential outcome columns. They should be named ",
                  paste0(paste0(outcome_variable_name, "_", assignment_variable_name, "_", conditions_without_columns), collapse = " and "), ". "))
    }

    data[, outcome_variable_name] <- NA
    for(cond in condition_names_assignment_variable){
      data[data[, assignment_variable_name] == cond, outcome_variable_name] <-
        data[data[, assignment_variable_name] == cond, paste0(outcome_variable_name, "_", assignment_variable_name, "_", cond)]
    }

    return(data)

}

attributes(reveal_outcomes) <- list(type = "reveal_outcomes")

# get vector of condition_names_pos
# get vector of condition_names_assignment_variable_name
# check if any *assignment_variable_name* condition names are not among the pos
# if so, error
# else, swtiching eq
