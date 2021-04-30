#' Declare inquiry
#'
#' @description Declares inquiries, or the inferential target of interest. Conceptually very close to "estimand" or "quantity of interest".
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function, I(), that accepts a data.frame as an argument and returns a data.frame containing the value of the inquiry,  a^m. 
#'
#' @details
#'
#' For the default diagnosands, the return value of the handler should have \code{inquiry_label} and \code{estimand} columns.
#'
#' @export
#'
#' @examples
#'
#' # Set up a design for use in examples:
#'
#' design <-
#'   declare_model(N = 100,
#'                 X = rnorm(N),
#'                 potential_outcomes(Y ~ (.25 + X) * Z + rnorm(N))) +
#'   declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#' 
#' design + declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#' 
#' 
#' design + declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0),
#'                          subset = (Z == 1))
#' 
#' # Add inquirys to a design along with estimators that reference them
#' 
#' design_1 <-
#'   design +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' run_design(design_1)
#' 
#' # Two inquirys, one estimator
#' 
#' design_2 <-
#'   design +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0), subset = (Z == 1)) +
#'   declare_estimator(Y ~ Z, inquiry = c("ATE", "ATT"))
#' 
#' run_design(design_2)
#' 
#' # Two inquirys, two coefficients from one estimator
#' 
#' design_3 <-
#'   design +
#'   declare_inquiry(intercept = mean(Y_Z_0),
#'                   slope = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_estimator(
#'     Y ~ Z,
#'     model = lm_robust,
#'     term = TRUE,
#'     inquiry = c("intercept", "slope")
#'   )
#' 
#' run_design(design_3)
declare_inquiry <- make_declarations(inquiry_handler, "inquiry",
                                      causal_type = "inquiry", 
                                      default_label = "inquiry"
)

#' @rdname declare_inquiry
#' @export
declare_inquiries <- declare_inquiry

#' @rdname declare_inquiry
#' @export
declare_estimand <-  function(...){
  .Deprecated(new = "declare_inquiry")
  declare_inquiry(...)
}

#' @rdname declare_inquiry
#' @export
declare_estimands <- function(...){
  .Deprecated(new = "declare_inquiry")
  declare_inquiry(...)
}


#' @param subset a subset expression
#' @param term TRUE/FALSE
#' @param data a data.frame
#' @details
#'
#' If term is TRUE, the names of ... will be returned in a \code{term} column,
#' and \code{inquiry_label} will contain the step label. This can be used as
#' an additional dimension for use in diagnosis.
#'
#'
#' @importFrom rlang eval_tidy quos is_quosure quo_get_expr as_data_mask
#' @rdname declare_inquiry
inquiry_handler <- function(data, ..., subset = NULL, term = FALSE, label) {
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
      inquiry_label = label,
      term = names(options),
      estimand = ret,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    data.frame(
      inquiry_label = names(options),
      estimand = ret,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
}

validation_fn(inquiry_handler) <- function(ret, dots, label) {
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)
  
  declare_time_error_if_data(ret)
  
  # Don't overwrite label-label with splat label if term names are true
  if ("term" %in% dotnames && isTRUE(eval_tidy(dots$term))) {
    return(ret)
  }
  
  maybeDotLabel <- dotnames[!dotnames %in% c("", names(formals(inquiry_handler)))]
  if (any(duplicated(maybeDotLabel))) {
    stop(paste0(
      "Please provide unique names for each inquiry. Duplicates include ",
      paste(maybeDotLabel[duplicated(maybeDotLabel)], collapse = ", "), "."
    ), call. = FALSE)
  }
  if (length(maybeDotLabel) == 1) {
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }
  
  ret
}
