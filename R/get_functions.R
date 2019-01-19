#' @param data A data.frame object with sufficient information to conduct data generation from the start step to the end step.
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin drawing data from. By default all data steps are drawn, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish drawing data by.
#'
#' @rdname post_design
#' @export
get_data <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument.")
  }
  
  if(start < 1 || start > length(design)){
    stop("Please provide a starting step as a number between 1 and the total number of steps in the design.")
  }
  
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }
  
  run_design_internal.design(design[start:end], current_df = data, results = list(current_df = 0))$current_df
  
}

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

#' @param data A data.frame object with sufficient information to draw assignment vectors. 
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin drawing assignment vectors. By default all assignment vectors are drawn, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish drawing assignment vectors. 
#'
#' @rdname post_design
#' @export
get_assignment <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument.")
  }
  
  if(start < 1 || start > length(design)){
    stop("Please provide a starting step as a number between 1 and the total number of steps in the design.")
  }
  
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }
  
  assignments <- Filter(function(x) attr(x, "step_type") == "assignment", design[start:end])
  run_design_internal.design(assignments, current_df = data, results = list(current_df = 0))$current_df
  
}

#' @param data A data.frame object with sufficient information to run sampling steps. 
#' @param start (Defaults to 1) a scalar indicating which step in the design to begin sampling. By default all sampling steps are run, from step 1 to the last step of the design.
#' @param end (Defaults to \code{length(design)}) a scalar indicating which step in the design to finish sampling. 
#'
#' @rdname post_design
#' @export
get_sample <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument.")
  }
  
  if(start < 1 || start > length(design)){
    stop("Please provide a starting step as a number between 1 and the total number of steps in the design.")
  }
  
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }

  samplings <- Filter(function(x) attr(x, "step_type") == "sampling", design[start:end])
  run_design_internal.design(samplings, current_df = data, results = list(current_df = 0))$current_df
  
}
