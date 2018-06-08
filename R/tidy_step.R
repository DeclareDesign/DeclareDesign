#' Convert a step to a tidy function for use in a design
#'
#' Wrap a data-in, data-out function call as a tidy function
#'
#' @param ... A function of data and other arguments that returns data
#'
#' @return A function that takes only data compatible with creating a design
#' @export
#'
#' @examples
#' 
#' # With dplyr verbs like mutate
#' library(dplyr)
#' my_population <- declare_population(N = 10)
#' a_wrapped_mutate <- tidy_step(mutate(q = 5))
#' 
#' my_design <- my_population + a_wrapped_mutate
#' my_design
#' 
#' # With user-written functions
#' 
#' my_function <- function(data, my_mean) {
#' data$new_variable <- rnorm(n = nrow(data), mean = my_mean)
#' data
#' }
#' 
#' my_design <- my_population + tidy_step(my_function(my_mean = 5))
#' my_design
#' 
tidy_step <- function(...){
  qs <- quo(...)
  qnames <- names(qs)

  # TODO: look into qs to see if there is an object that already inherits design_step 
  
  ret <- tryCatch(
    eval_tidy(qs),
    error = function(e) tryCatch(callquos_to_step(qs, qnames),
                                 error = function(e) stop("Could not evaluate step `", qnames,
                                                          "` as either a step or call. Does the object exist?"))
  )
  
  # Is it a non-declared function
  if (is.function(ret) && !inherits(ret, "design_step")) {
    if (!any(names(formals(ret)) %in% c("data", ".data"))) {
      stop("The arguments of the tidy'd function do not include data or .data.", call. = FALSE)
    }
    ret <- build_step(
      ret, handler = NULL, dots = list(), label = qnames, step_type = "undeclared", causal_type = "dgp", call = qs[[2]]
    )
  }
  
  ret
}