
#' Declare Design
#'
#' @param ... A set of steps in a research design, beginning with a \code{data.frame} representing the population or a function that draws the population. Steps are evaluated sequentially. With the exception of the first step, all steps must be functions that take a \code{data.frame} as an argument and return a \code{data.frame}. Typically, many steps are declared using the \code{declare_} functions, i.e., \code{\link{declare_population}}, \code{\link{declare_population}}, \code{\link{declare_sampling}}, \code{\link{declare_potential_outcomes}}, \code{\link{declare_estimand}}, \code{\link{declare_assignment}}, and \code{\link{declare_estimator}}. Functions from the \code{dplyr} package such as mutate can also be usefully included.
#'
#' @details
#'
#' Users can supply three kinds of functions to declare_design:
#'
#' 1. Data generating functions. These include population, assignment, and sampling functions.
#'
#' 2. Estimand functions.
#'
#' 3. Estimator functions.
#'
#' The location of the estimand and estimator functions in the chain of functions determine *when* the values of the estimand and estimator are calculated. This allows users to, for example, differentiate between a population average treatment effect and a sample average treatment effect by placing the estimand function before or after sampling.
#'
#' Designs declared with declare_design can be investigated with a series of post-declaration commands, such as \code{\link{draw_data}}, \code{\link{get_estimands}}, \code{\link{get_estimates}}, and \code{\link{diagnose_design}}.
#'
#' The print and summary methods for a design object return some helpful descriptions of the steps in your research design. If randomizr functions are used for any assignment or sampling steps, additional details about those steps are provided.
#'
#' @return a list of two functions, the \code{design_function} and the \code{data_function}. The \code{design_function} runs the design once, i.e. draws the data and calculates any estimates and estimands defined in \code{...}, returned separately as two \code{data.frame}'s. The \code{data_function} runs the design once also, but only returns the final data.
#'
#' @importFrom rlang quos quo_expr eval_tidy quo_text lang_args is_formula
#' @importFrom utils bibentry
#' @export
#'
#' @examples
#'
#' my_population <- declare_population(N = 500, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(Y ~ Z + noise)
#'
#' my_sampling <- declare_sampling(n = 250)
#'
#' my_assignment <- declare_assignment(m = 25)
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' my_reveal <- declare_reveal()
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_sampling,
#'                          my_estimand,
#'                          dplyr::mutate(noise_sq = noise^2),
#'                          my_assignment,
#'                          my_reveal,
#'                          my_estimator)
#'
#' design
#'
#' df <- draw_data(design)
#'
#' estimates <- get_estimates(design)
#' estimands <- get_estimands(design)
#'
#' \dontrun{
#' diagnosis <- diagnose_design(design)
#'
#' summary(diagnosis)
#' }
#'
declare_design <- function(...) {
  
  qs <- quos(...)
  qs <- maybe_add_labels(qs)
  
  declare_design_internal(!!!qs)
  
}


declare_design_internal <- function(...) {
  
  qs <- quos(...)
  
  ret <- structure(vector("list", length(qs)),
                   call = match.call(),
                   class = c("design", "d_par"))
  
  qnames <- names(qs)
  names(ret)[qnames != ""] <- qnames[qnames != ""]
  
  # for each step in qs, eval, and handle edge cases (dplyr calls, non-declared functions)
  for (i in seq_along(qs)) {
    
    # check if object exists
    
    # wrap step is nasty, converts partial call to curried function
    ret[[i]] <- tryCatch(
      eval_tidy(qs[[i]]),
      error = function(e) tryCatch(callquos_to_step(qs[[i]], qnames[[i]]),
                                   error = function(e) stop("Could not evaluate step `", qnames[[i]],
                                                            "` as either a step or call. Does the object exist?"))
    )
    
    # Is it a non-declared function
    if (is.function(ret[[i]]) && !inherits(ret[[i]], "design_step")){
      if (!identical(names(formals(ret[[i]])), "data")){
        warning("Undeclared Step ", i, " function arguments are not exactly 'data'")
      }
      
      ret[[i]] <- build_step(
        ret[[i]],
        handler=NULL, dots=list(), label=qnames[i], step_type="undeclared", causal_type="dgp", call=qs[[i]][[2]]
      )
    }
    
  }
  
  # Special case for initializing with a data.frame
  if(inherits(ret[[1]], "data.frame")){
    # stop("Please do not provide a data.frame. You can declare a population via declare_population(data = my_data).")
    ret[[1]] <- build_step(
      (function(df) { force(df); function(data) df})(ret[[1]]),
      handler=NULL, dots=list(), label=qnames[1], step_type="seed_data", causal_type="dgp", call=qs[[1]][[2]]
    )
    class(ret[[1]]) <- c("seed_data", class(ret[[1]]))
  }
  
  # Assert that all labels are unique
  local({
    labels <- sapply(ret, attr, "label")
    function_types <- sapply(ret, attr, "step_type")
    
    check_unique_labels <- function(labels, types, what) {
      ss <- labels[types == what]
      if(anyDuplicated(ss)) stop(
        "You have ", what, "s with identical labels: ",
        unique(ss[duplicated(ss)]),
        "\nPlease provide ", what, "s with unique labels"
      )
    }
    
    check_unique_labels(labels, function_types, "estimand")
    check_unique_labels(labels, function_types, "estimator")
    
  })
  
  # If there is a design-time validation, trigger it
  for(i in seq_along(ret)){
    step <- ret[[i]]
    callback <- attr(step, "design_validation")
    if(is.function(callback)){
      ret <- callback(ret, i, step)
    }
  }
  
  ret
}

###############################################################################
# In declare_design, if a step is a dplyr style call mutate(foo=bar),
# it will fail to evaluate - we can catch that and try to curry it

#' @importFrom rlang quos lang_fn lang_modify eval_tidy
callquos_to_step <- function(step_call, label="") {
  ## this function allows you to put any R expression
  ## such a dplyr::mutate partial call
  ## into the causal order, i.e.
  ## declare_design(pop(), po, mutate(q = 5))
  
  # match to .data or data, preference to .data
  data_name <- intersect(names(formals(lang_fn(step_call))), c(".data", "data") )[1]
  
  stopifnot(is.character(data_name))
  
  # splits each argument in to own quosure with common parent environment
  dots <- lapply(step_call[[2]], function(d){
    dot <- step_call
    dot[[2]] <- d
    dot
  })
  
  fun <- eval_tidy(dots[[1]])
  dots <- dots[-1]
  
  dots <- quos(!!data_name := data, !!!dots)
  
  curried <- currydata(fun, dots)
  
  build_step(
    curried,
    handler = fun,
    dots = dots,
    label,
    step_type = "wrapped",
    causal_type = "dgp",
    call = step_call
  )
  
}