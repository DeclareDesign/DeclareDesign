#' Declare data strategy
#'
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#' 
declare_data_strategy <- make_declarations(data_strategy_handler, "data_strategy")

#' @param data A data.frame.
#' @importFrom rlang quos !!! quo_get_expr eval_tidy is_quosure 
#' @importFrom fabricatr fabricate
#' @rdname declare_data_strategy
data_strategy_handler <- function(data, ..., subset = NULL) {
  
  options <- quos(...)
  
  data <- fabricate(data = data, !!!options, ID_label = NA)
  
  subset <- substitute(subset)
  if (is_quosure(subset)) subset <- quo_get_expr(subset)
  idx <- eval_tidy(subset, data = data)
  if (!is.null(idx)) {
    data <- data[idx, , drop = FALSE]
  }
  
}
