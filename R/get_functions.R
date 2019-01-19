
#' @param data A data.frame object with sufficient information to run estimators. 
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin getting estimates from. By default all estimators are calculated, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish getting estimates from. 
#'
#' @rdname post_design
#' @export
get_estimates <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
  }
  
  estimators <- Filter(function(x) attr(x, "causal_type") == "estimator", design[start:end])
  run_design_internal.design(estimators, current_df = data)$estimates_df
  
}
