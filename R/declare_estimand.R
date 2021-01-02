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
#' For the default diagnosands, the return value of the handler should have \code{inquiry_label} and \code{inquiry_value} columns.
#'
#' @export
#'
#' @examples
#'
#' # Set up a design stub for use in examples:
#'
#' design_stub  <- 
#'   declare_model(
#'     N = 100, 
#'     X = rnorm(N),
#'     potential_outcomes(Y ~ Z + rnorm(N))
#'   ) + 
#'   declare_assignment(Z = complete_ra(N, m = 50)) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#'
#' # sGet example data to compute inquiries on
#' dat <- draw_data(design_stub)
#'
#' # ----------
#' # 1. Single inquiry
#' # ----------
#'
#' # Declare an average treatment effect (ATE) inquiry
#' 
#' my_inquiry_ATE <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#' my_inquiry_ATE(dat)
#' 
#' # or a conditional inquiry
#' 
#' my_inquiry_ATT <- declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0),
#'                                   subset = (Z == 1))
#' my_inquiry_ATT(dat)
#' 
#' # Add inquiries to a design along with answer strategies 
#' # (estimators) that reference them
#' 
#' my_estimator <- declare_estimator(Y ~ Z, inquiry = "ATE")
#'
#' design_one <- design_stub + my_inquiry_ATE + my_estimator
#'
#' draw_inquiries(design_one)
#'
#' # ----------
#' # 2. Multiple inquiries
#' # ----------
#'
#' # You can also specify multiple inquiries for a single estimator 
#'
#' my_estimator_two <- declare_estimator(Y ~ Z, 
#' inquiry = c("ATE", "ATT"))
#'
#' design_two <- design_stub + my_inquiry_ATE + 
#'   my_inquiry_ATT + my_estimator_two
#' 
#' draw_inquiries(design_two)
#'
#' # ----------
#' # 3. Paired inquiries / estimators from a single model
#' # ----------
#'
#' # For convenience you can also declare multiple inquiries
#' # simultaneously and connect these to the corresponding 
#' # terms in the estimator. 
#'
#' # Name your inquiries the term name they get in your
#' # estimator, and set `term = TRUE`
#'
#' inquiries_regression <- declare_inquiries(
#'   `(Intercept)` = mean(Y_Z_0),
#'   `Z` = mean(Y_Z_1 - Y_Z_0),
#'   term = TRUE,
#'   label="Regression_inquiries"
#' )
#'
#' # For the model based estimator, specify the inquiry as usual,
#' # but also set `term = TRUE`
#' estimators_regression <- declare_estimator(
#'   Y ~ Z,
#'   inquiry = inquiries_regression,
#'   model = lm,
#'   term = TRUE
#' )
#'
#' design_regression <- design_stub + inquiries_regression +
#'   estimators_regression
#'
#' run_design(design_regression)
#'
#' # ----------
#' # 4. Custom inquiry function
#' # ----------
#'
#' # You can declare more complex inquiries by defining custom
#' # inquiry functions:
#' 
#' inquiry_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(inquiry_label = label,
#'              inquiry_value = ret,
#'              stringsAsFactors = FALSE)
#' }
#' 
#' inquiry_custom <- declare_inquiry(handler = inquiry_function,
#'   label = "medianTE")
#'
#' inquiry_custom(dat)
#'
#' # Use with custom estimators
#' estimator_function <- function(data){
#'   data.frame(estimate = with(data, median(Y)))
#' }
#' estimator_custom <-
#'   declare_estimator(handler = tidy_estimator(estimator_function),
#'                     inquiry = inquiry_custom)
#'
#' design_custom <- design_stub + inquiry_custom +
#'   estimator_custom
#' 
#' run_design(design_custom)
#'
#' # ----------
#' # 5. Batch inquiries and estimators
#' # ----------
#' 
#' # You can declare a group of inquiries with distinct labels
#' # in one go and link them manually to a group of estimators.
#' # In this case you can add a \code{term} argument to the 
#' # custom estimators to identify them.
#' 
#' f1 <- function(data) {
#'   data.frame(inquiry_label = c("control", "ate"),
#'              inquiry_value = with(data, c(mean(Y_Z_0), mean(Y_Z_1 - Y_Z_0))),
#'              stringsAsFactors = FALSE)
#' }
#' inquiries <- declare_inquiry(handler = f1)
#' 
#' f2 <- function(data) {
#'   data.frame(estimate = 
#'                with(data, c(mean(Y[Z == 0]),
#'                             mean(Y[Z == 1]) - mean(Y[Z == 0]))),
#'              term = 1:2)
#' }
#' 
#' estimators <- declare_estimator(handler = label_estimator(f2), 
#'                             inquiry = c("control", "ate"), label = "custom")
#'                             
#' design   <- design_stub + inquiries + estimators
#' 
#' \dontrun{
#' diagnose_design(design, sims = 20, bootstrap_sims = FALSE,
#'                 diagnosands = declare_diagnosands(
#'                 select = c(mean_estimate, mean_inquiry_value)))
#' }
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
      inquiry_value = ret,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    data.frame(
      inquiry_label = names(options),
      inquiry_value = ret,
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
