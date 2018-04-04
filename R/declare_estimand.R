


#' Declare Estimand
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that accepts a data.frame as an argument and returns a data.frame containing the value of the estimand.
#'
#' @export
#'
#' @examples
#'
#' ################################
#' # Default handler
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimand <- declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0), subset=Z==1)
#'
#' my_estimand <- declare_estimand(
#'   `(Intercept)`=1,
#'   Z=2,
#'   coefficients=TRUE,
#'   label="TrueRegressionParams"
#' )
#'
#' ################################
#' # Custom random assignment functions
#'
#' my_estimand_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(estimand_label=label,
#'              estimand=ret,
#'              time=Sys.time(),
#'              stringsAsFactors=FALSE)
#' }
#' my_estimand_custom <- declare_estimand(handler = my_estimand_function, label = "medianTE")
#'

declare_estimand <- make_declarations(estimand_handler, "estimand", causal_type="estimand", default_label="my_estimand")

#' @param subset a subset expression
#' @param coefficients TRUE/FALSE
#' @param data a data.frame
#' @details
#'
#' If coefficients is TRUE, the names of ... will be returned in a `coefficients` column, and `estimand_label`
#' will contain the step label. This can be used as an additional dimension for use in diagnosis.
#'
#'
#' @importFrom rlang eval_tidy quos  is_quosure
#' @rdname declare_estimand
estimand_handler <- function(data, ..., subset = NULL, coefficients=FALSE, label) {
  options <- quos(...)
  if(names(options)[1] == "") names(options)[1] <- label

  subset <- substitute(subset)
  if(is_quosure(subset)) subset <- subset[[2]]
  idx <- eval_tidy(subset, data = data)
  if (!is.null(idx)) {
    data <- data[idx, , drop = FALSE]
  }

  ret <- vector("list", length(options))
  for(i in seq_along(options)){
    ret[i] <- eval_tidy(options[[i]], data=data)
  }
  ret <- simplify2array(ret)

  if(coefficients){
    data.frame(estimand_label=label,
               coefficient=names(options),
               estimand=ret,
               stringsAsFactors = FALSE)


  } else {
    data.frame(estimand_label=names(options),
               estimand=ret,
               stringsAsFactors = FALSE)
  }
}

validation_fn(estimand_handler) <-  function(ret, dots, label){
  force(ret)
  # add ... labels at build time
  dotnames <- names(dots)

  declare_time_error_if_data(ret)


  # Don't overwrite label-label with splat label if coefficient names are true
  if("coefficients" %in% dotnames && isTRUE(eval_tidy(dots$coefficients))) return(ret)

  maybeDotLabel <- dotnames[! dotnames %in% c("", names(formals(estimand_handler)) )]
  if(length(maybeDotLabel) == 1 ){
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }

  ret
}
