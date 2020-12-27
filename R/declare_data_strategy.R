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
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_data_strategy
data_strategy_handler <- function(data, ...) {
  
  options <- quos(...)
  
  fabricate(data = data, !!!options, ID_label = NA)
  
}
