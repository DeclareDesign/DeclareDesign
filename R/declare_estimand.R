


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
#' my_population <- declare_population(N = 100)
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   formula = Y ~ .25 * Z,
#'   conditions = c(0, 1))
#'
#' df <- my_potential_outcomes(my_population())
#'
#' # Use the default estimand setup to
#' # declare an average treatment effect estimand
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimand(df)
#'
#' # Custom random assignment functions
#'
#' my_estimand_function <- function(data, label) {
#'   ret <- with(data, median(Y_Z_1 - Y_Z_0))
#'   data.frame(setNames(ret, label))
#' }
#' my_estimand_custom <- declare_estimand(
#'   handler = my_estimand_function, label = "medianTE")
#'
#' my_estimand_custom(df)
#'

declare_estimand <- make_declarations(estimand_handler, "estimand", causal_type="estimand", default_label="my_estimand")


#' @importFrom rlang eval_tidy quos  is_quosure
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

  # Don't overwrite label-label with splat label if coefficient names are true
  if("coefficients" %in% dotnames && isTRUE(eval_tidy(dots$coefficients))) return(ret)

  maybeDotLabel <- dotnames[! dotnames %in% c("", names(formals(estimand_handler)) )]
  if(length(maybeDotLabel) == 1 ){
    attr(ret, "steplabel") <- attr(ret, "label")
    attr(ret, "label") <- maybeDotLabel[1]
  }

  ret
}
