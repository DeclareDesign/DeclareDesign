

#' Declare Assignment Procedure
#'
#' @param ... Arguments to the assignment function.
#' @param handler A function that takes a data.frame, adds an assignment variable and optionally assignment probabilities or other relevant quantities, and returns a data.frame. By default, the assignment_function uses the \link{randomizr} functions \code{\link{conduct_ra}} and \code{\link{obtain_condition_probabilities}} to conduct random assignment and obtain the probabilities of assignment to each condition.
#' @param label a step label
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame with additional columns appended including an assignment variable and (optionally) probabilities of assignment.
#' @export
#'
#' @details
#'
#' While declare_assignment can work with any assignment_function that takes data and returns data, most random assignment procedures can be easily implemented with randomizr. The arguments to \code{\link{conduct_ra}} can include N, block_var, clust_var, m, m_each, prob, prob_each, block_m, block_m_each = NULL, block_prob, block_prob_each, num_arms, and conditions. The arguments you need to specify are different for different designs. Check the help files for \code{\link{complete_ra}}, \code{\link{block_ra}}, \code{\link{cluster_ra}}, or \code{\link{block_and_cluster_ra}} for details on how to execute many common designs.
#'
#' @importFrom rlang quos quo lang_modify eval_tidy !!!
#' @importFrom randomizr declare_ra
#'
#' @examples
#'
#' my_population <- declare_population(N = 100, female = rbinom(N, 1, .5))
#' df <- my_population()
#'
#' # Complete random assignment using randomizr
#' # use any arguments you would use in conduct_ra.
#'
#' my_assignment <- declare_assignment(m = 50)
#' df <- my_assignment(df)
#' head(df)
#' table(df$Z)
#'
#' # Block random assignment
#'
#' my_blocked_assignment <- declare_assignment(blocks = female)
#'
#' df <- my_population()
#'
#' df <- my_blocked_assignment(df)
#' head(df)
#' with(df, table(Z, female))
#'
#'
#' # Custom random assignment functions
#'
#' df <- my_population()
#'
#' my_assignment_function <- function(data) {
#'    data$Z <- rbinom(n = nrow(data),
#'    size = 1,
#'    prob = 0.5)
#'    data
#'    }
#'
#' my_assignment_custom <- declare_assignment(
#'    handler = my_assignment_function)
#'
#' df <- my_assignment_custom(df)
#' head(df)
#' table(df$Z)
declare_assignment <- make_declarations(assignment_handler, "assignment" )


#' @importFrom rlang quos !!! lang_modify eval_tidy quo f_rhs
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
assignment_handler <-
  function(data, ..., assignment_variable = Z, reveal="auto") {
    ## draw assignment

    options <- quos(...)

    assignment_variable <- reveal_nse_helper(enquo(assignment_variable))

    ra_call <- quo(conduct_ra(!!! options))
    ra_call <- lang_modify(ra_call, N = nrow(data))

    data[, assignment_variable] <- eval_tidy(ra_call, data = data)

    ## obtain condition probabilities

    prob_call <- quo(obtain_condition_probabilities(!!! options))
    prob_call <- lang_modify(prob_call, assignment = data[, assignment_variable])

    data[, paste0(assignment_variable, "_cond_prob")] <-
      eval_tidy(prob_call, data = data)

    outcome <- attr(data, "outcome_variable_name")
    if(reveal == "auto"
       && is.character(outcome) &&
       assignment_variable == attr(data, "assignment_variable")) {
          data <- reveal_outcomes(data, !!outcome, !!assignment_variable)
    }

    return(data)

  }

validation_fn(assignment_handler) <-   function(ret, dots, label){

  if ("blocks" %in% names(dots)) {
    if (class(f_rhs(dots[["blocks"]])) == "character") {
      declare_time_error("Must provide the bare (unquoted) block variable name to blocks.", ret)
    }
  }

  if ("clusters" %in% names(dots)) {
    if (class(f_rhs(dots[["clusters"]])) == "character") {
      declare_time_error("Must provide the bare (unquoted) cluster variable name to clusters.", ret)
    }
  }

  if("assignment_variable" %in% names(dots)){
    if (class(f_rhs(dots[["assignment_variable"]])) == "NULL") {
      declare_time_error("Must provide assignment_variable.", ret)
    }
  }

  ret
}


