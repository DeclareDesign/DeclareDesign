
#' @export
declare_statistical_model <- make_declarations(statistical_model_handler, step_type = "wallet", causal_type = "estimator")

#' @importFrom rlang eval_tidy quos as_label
#' @export
statistical_model_handler <- function(data, ...) {
  
  dots <- quos(...)
  
  if(length(dots) > 1) {
    stop("Please only send one model to declare_statistical_model.")
  }
  
  expr_quo <- dots[[1]]
  
  # label estimator
  nm <- names(dots)[1]
  if(nm == "") {
    nm <- as_label(expr_quo)
  }
  
  mod <- list()
  mod[[nm]] <- eval_tidy(expr_quo, data = data)
  mod
  
}
