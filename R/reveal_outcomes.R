
#' Declare a Reveal Outcomes step
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
declare_reveal <- make_declarations(reveal_outcomes, "reveal_outcomes")


#' @param data A data.frame containing columns of potential outcomes and an assignment variable
#'
#' @param outcome_variables The outcome prefix(es) of the potential outcomes
#' @param assignment_variables The bare (unquote) name(s) of the assignment variable
#' @param attrition_variables The bare (unquote) name of the attrition variable
#'
#' @details
#'
#' Typically, a design includes a potential outcomes declaration and an assignment declaration. Reveal outcomes uses the random assignment to pluck out the correct potential outcomes. This is analogous to the "switching equation" (Gerber and Green 2012, Chapter 2).
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_assignment,
#'                          reveal_outcomes)
#'
#' design
reveal_outcomes <-
  function(data = NULL,
           outcome_variables = Y,
           assignment_variables = Z,
           attrition_variables = NULL) {

    if(!is.data.frame(data)) {
      stop("Please provide data to reveal_outcomes.")
    }

    # Setup to handle NSE
    outcome <- reveal_nse_helper(enquo(outcome_variables))
    attrition <- reveal_nse_helper(enquo(attrition_variables))
    assignment <- reveal_nse_helper(enquo(assignment_variables))

    for (i in seq_along(outcome)) {
      data[, outcome[i]] <- switching_equation(data, outcome[i], assignment)
    }

    for (i in seq_along(attrition)) {
      response  <- switching_equation(data, attrition[i], assignment)
      data[response == 0, outcome[i]] <- NA
    }

    return(data)
}

attributes(reveal_outcomes) <- list(step_type = "reveal_outcomes",
                                    causal_type= "dgp",
                                    call=quote(reveal_outcomes()),
                                    class=c("design_step", "function"))

validation_fn(reveal_outcomes) <- function(ret, dots, label) {
  if("attrition_variable" %in% names(dots)) {


  }
  ret
}


switching_equation <- function(data, outcome, assignments) {

  potential_outcome_columns <- mapply(paste, assignments, data[,assignments, drop=FALSE],   sep="_", SIMPLIFY = FALSE)
  potential_outcome_columns <- do.call(paste, c(outcome, potential_outcome_columns, sep="_"))

  upoc <- unique(potential_outcome_columns)

  if(!(all(upoc %in% colnames(data)))){
    stop(
      "Must provide all potential outcomes columns referenced by the assignment variable (", assignments, ").\n",
      "`data` did not include:\n",
      paste("  * ", sort(setdiff(upoc, colnames(data))), collapse="\n")
    )
  }

  data <- data[ , upoc, drop=FALSE]

  R <- 1:nrow(data)
  C <- match(potential_outcome_columns, colnames(data))

  data[cbind(R,C)]

}


reveal_nse_helper <- function(X) {
  if(is.character(X))     X
  else if(is.name(X))     as.character(X)
  else if(is_quosure(X))  reveal_nse_helper(quo_expr(X))
  else if(is.call(X))     unlist(lapply(X[-1], reveal_nse_helper))
}
