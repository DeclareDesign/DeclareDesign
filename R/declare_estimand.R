#' Declare estimand
#'
#' @description Declares estimands. Estimands are the subjects of inquiry and can be estimated by an estimator.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return an estimand declaration, which is a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @details
#'
#' For the default diagnosands, the return value of the handler should have \code{estimand_label} and \code{estimand} columns.
#'
#' @export
#'
#' @examples
#'
#' # Set up a design for use in examples:
#' 
#' design <-
#'   declare_population(N = 100, X = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ (.25 + X) * Z + rnorm(N)) +
#'   declare_assignment(m = 50) 
#' 
#' design + declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#' 
#' 
#' design + declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0),
#'                                    subset = (Z == 1))
#' 
#' # Add estimands to a design along with estimators that reference them
#' 
#' design_1 <-
#'   design +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_estimator(Y ~ Z, estimand = "ATE")
#' 
#' run_design(design_1)
#' 
#' # Two estimands, one estimator
#' 
#' design_2 <-
#'   design +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0), subset = (Z == 1)) + 
#'   declare_estimator(Y ~ Z, estimand = c("ATE", "ATT"))
#' 
#' run_design(design_2)
#' 
#' # Two estimands, two coefficients from one estimator
#' 
#' design_3 <-
#'   design +
#'   declare_estimand(intercept = mean(Y_Z_0),
#'                    slope = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_estimator(
#'     Y ~ Z,
#'     model = lm_robust,
#'     term = TRUE,
#'     estimand = c("intercept", "slope")
#'   )
#' 
#' run_design(design_3)
#' 
declare_estimand <- make_declarations(estimand_handler, "estimand",
                                      causal_type = "estimand", 
                                      default_label = "estimand"
)

#' @rdname declare_estimand
#' @export
declare_estimands <- declare_estimand

#' @param subset a subset expression
#' @param term TRUE/FALSE
#' @param data a data.frame
#' @details
#'
#' If term is TRUE, the names of ... will be returned in a \code{term} column,
#' and \code{estimand_label} will contain the step label. This can be used as
#' an additional dimension for use in diagnosis.
#'
#'
#' @importFrom rlang eval_tidy quos is_quosure quo_get_expr as_data_mask
#' @rdname declare_estimand
estimand_handler <- function(data, ..., subset = NULL, term = FALSE, label) {
  options <- quos(...)
  if (names(options)[1] == "") names(options)[1] <- label
  
  subset <- substitute(subset)
  if (is_quosure(subset)) subset <- quo_get_expr(subset)
  idx <- eval_tidy(subset, data = data)
  if (!is.null(idx)) {
    data <- data[idx, , drop = FALSE]
  }
  
#  ret <- vector("list", length(options))
  ret <- as_data_mask(data)
  for (i in names(options)) {
    ret[[i]] <- eval_tidy(options[[i]], data = ret)
  }
  ret <- mget(names(options), ret)
  ret <- simplify2array(ret)
  
  if (term) {
    data.frame(
      estimand_label = label,
      term = names(options),
      estimand = ret,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    data.frame(
      estimand_label = names(options),
      estimand = ret,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
}

validation_fn(estimand_handler) <- function(ret, dots, label) {
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)
  
  declare_time_error_if_data(ret)
  
  # Don't overwrite label-label with splat label if term names are true
  if ("term" %in% dotnames && isTRUE(eval_tidy(dots$term))) {
    return(ret)
  }
  
  maybeDotLabel <- dotnames[!dotnames %in% c("", names(formals(estimand_handler)))]
  if (any(duplicated(maybeDotLabel))) {
    stop(paste0(
      "Please provide unique names for each estimand. Duplicates include ",
      paste(maybeDotLabel[duplicated(maybeDotLabel)], collapse = ", "), "."
    ), call. = FALSE)
  }
  if (length(maybeDotLabel) == 1) {
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }
  
  ret
}
