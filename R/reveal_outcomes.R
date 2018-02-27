
#' Declare a Reveal Outcomes step
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
reveal_outcomes_handler <-
  function(data = NULL,
           outcome_variables = Y,
           assignment_variables = Z,
           attrition_variables = NULL,
           ...) {

    if(!is.data.frame(data)) {
      stop("Please provide data to reveal_outcomes.")
    }

    if(!is.character(outcome_variables)) {
      stop("outcome_variables should already be converted to characters")
    }
    if(!is.character(assignment_variables)) {
      stop("assignment_variables should already be converted to characters")
    }
    if(!is.null(attrition_variables) && !is.character(assignment_variables)) {
      stop("attrition_variables should already be converted to characters")
    }

    for (i in seq_along(outcome_variables)) {
      data[, outcome_variables[i]] <- switching_equation(data, outcome_variables[i], assignment_variables)
    }

    for (i in seq_along(attrition_variables)) {
      response  <- switching_equation(data, attrition_variables[i], assignment_variables)
      data[response == 0, outcome_variables[i]] <- NA
    }

    return(data)
}

# attributes(reveal_outcomes) <- list(step_type = "reveal_outcomes",
#                                     causal_type= "dgp",
#                                     call=quote(reveal_outcomes()),
#                                     class=c("design_step", "function"))

validation_fn(reveal_outcomes_handler) <- function(ret, dots, label) {
  dots$outcome_variables <- if("outcome_variables" %in% names(dots)) {
     reveal_nse_helper(dots$outcome_variables)
  } else as.character(formals(reveal_outcomes_handler)$outcome_variables)

  dots$assignment_variables <- if("assignment_variables" %in% names(dots)) {
    reveal_nse_helper(dots$assignment_variables)
  } else as.character(formals(reveal_outcomes_handler)$assignment_variables)

  if("attrition_variable" %in% names(dots)) {
    dots$attrition_variable <- reveal_nse_helper(dots$attrition_variable)
  }

  if("deprecated" %in% names(dots)){
    .Deprecated("declare_reveal", old="reveal_outcomes")
    dots$deprecated <- NULL
  }

  build_step(currydata(reveal_outcomes_handler, dots, strictDataParam=attr(ret, "strictDataParam")),
             handler=reveal_outcomes_handler,
             dots=dots,
             label=label,
             step_type=attr(ret, "step_type"),
             causal_type=attr(ret,"causal_type"),
             call=attr(ret, "call"))
}

reveal_outcomes <- declare_reveal(deprecate=TRUE)

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
