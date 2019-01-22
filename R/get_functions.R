#' Get data, estimates, estimands, assignment vectors, or samples from a design given data
#'  
#' @param design A design object, typically created using the + operator
#' @param data A data.frame object with sufficient information to get the data, estimates, estimands, an assignment vector, or a sample.
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin with. By default all data steps are drawn, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish with.
#'
#' @examples
#' 
#' design <- declare_population(N = 100, u = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ Z + u) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(n = 75) +
#'   declare_assignment(m = 50) +
#'   declare_reveal(Y, Z) +
#'   declare_estimator(Y ~ Z, estimand = "ATE")
#' 
#' dat <- draw_data(design)
#' 
#' get_data(design, data = dat, start = 2)
#' 
#' get_estimates(design, data = dat)
#' 
#' get_assignment(design, data = dat)
#' 
#' get_sample(design, data = dat)
#'
#' @name get_functions
#' 
#' @export
get_data <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "causal_type") %in% "dgp")
}

#' @rdname get_functions
#' @export
get_estimates <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "causal_type") %in% "estimator")
}

#' @rdname get_functions
#' @export
get_assignment <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "step_type") %in% "assignment")
}

#' @rdname get_functions
#' 
#' @export
get_sample <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "step_type") %in% "sampling")
}

get_function_internal <- function(design, data = NULL, start, end, pred) {
  
  pred_str <- substitute(pred)
  
  type <- ifelse(pred_str == 'function(x) attr(x, "causal_type") == "estimator"', "get_estimates",
                 ifelse(pred_str == "function(x) TRUE", "draw_data", "other"))
  
  if(type != "draw_data") {
    if(is.null(data)){
      data_msg <- "Please provide a data frame to the data argument."
      if(type == "get_estimates") {
        data_msg <- paste(data_msg, "If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.", collapse = " ")
      }
      stop(data_msg)
    }
    
    if(start < 1 || start > length(design)){
      stop("Please provide a starting step as a number between 1 and the total number of steps in the design.")
    }
  }
  
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }
  
  design_subset <- Filter(pred, design[start:end])
  
  results_df <- list(current_df = 0)
  
  if(type == "get_estimates") {
    return(run_design_internal.design(design_subset, current_df = data)$estimates_df)
  } else {
    return(run_design_internal.design(design_subset, current_df = data, results = list(current_df = 0))$current_df)
  }
  
}

