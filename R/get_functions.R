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
#' get_sampling(design, data = dat)
#'
#' @name get_functions
#' 
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
  
  design_subset <- Filter(function(x) attr(x, "causal_type") == "dgp", design[start:end])
  
  run_design_internal.design(design_subset, current_df = data, results = list(current_df = 0))$current_df
  
}

#' @rdname get_functions
#' @export
get_estimates <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
  }
  
  estimators <- Filter(function(x) attr(x, "causal_type") == "estimator", design[start:end])
  run_design_internal.design(estimators, current_df = data)$estimates_df
  
}

#' @rdname get_functions
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

#' @rdname get_functions
#' 
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