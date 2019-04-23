#' Get estimates, estimands, assignment vectors, or samples from a design given data
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
#' draw_data(design, data = dat, start = 2)
#' 
#' get_estimates(design, data = dat)
#' 
#' draw_assignment(design, data = dat)
#' 
#' draw_sample(design, data = dat)
#'
#' @name get_functions

#' @rdname get_functions
#' @export
get_estimates <- function(design, data = NULL, start = 1, end = length(design)) {
  
  if(is.null(data)){
    stop("Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
  }
  
  get_function_internal(
    design, data, start, end, function(x) attr(x, "causal_type") %in% "estimator", NULL, "estimates_df", step_type = "estimator")
}

#' @rdname get_functions
#' @export
draw_assignment <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "step_type") %in% "assignment", step_type = "assignment")
}

#' @rdname get_functions
#' 
#' @export
draw_sample <- function(design, data = NULL, start = 1, end = length(design)) {
  get_function_internal(
    design, data, start, end, function(x) attr(x, "step_type") %in% "sampling", step_type = "sampling")
}


# utilities 

check_design_class_single <- function(design) {
  if(!inherits(design, "design"))
    stop("Please send a single design object to the design argument, typically created using the + operator.", call. = FALSE)
}

check_has_step_type <- function(design, step_type) {
  if(!is.null(step_type)){
    step_types <- sapply(design, function(x) attr(x, "step_type"))
    if(!any(step_types %in% step_type))
      stop(paste0("The design does not include any ", step_type, " steps."), call. = FALSE)
  }
}

get_function_internal <- function(design, data = NULL, start, end, pred, results = list(current_df = 0), what = "current_df", step_type = NULL) {
  
  check_design_class_single(design)
  
  if(identical(data, -9)){
    # Special NULL for draw_data
    data <- NULL
  } else if (!is.data.frame(data)) {
    stop("Please provide a data.frame to the data argument")
  }
  
  if(start < 1 || start > length(design)){
    stop("Please provide a starting step as a number between 1 and the total number of steps in the design.")
  }
  
  if(end < 1 || end > length(design)){
    stop("Please provide an end step as a number between 1 and the total number of steps in the design.")
  }
  
  design_subset <- Filter(pred, design[start:end])
  
  check_has_step_type(design_subset, step_type)
  
  ret <- run_design_internal.design(design_subset, current_df = data, results = results)[[what]]
  
  if(what == "estimates_df" && !is.null(ret$estimator_label) && typeof(ret$estimator_label) != "character"){
    warning("The estimator label should be a character, but it is a ", 
            class(ret$estimator_label), 
            ". Try using handler = tidy_estimator(your_estimator_function)", call. = FALSE)
  }
  
  if(what == "estimands_df" && !is.null(ret$estimand_label) && typeof(ret$estimand_label) != "character"){
    warning("The estimand label should be a character, but it is a ", 
            class(ret$estimand_label), 
            ". You may need stringsAsFactors = FALSE in your estimand function.", call. = FALSE)
  }
  
  ret

}