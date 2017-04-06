#' @export
declare_step <- function(...){

  ## this function allows you to put any R expression
  ## such a dplyr call mutate
  ## into the causal order, i.e.
  ## declare_design(pop(), po, declare_step(mutate(q = 5)))

  mcall <- lazy_dots(...)[[1]]

  arg_names <- names(formals(eval(mcall$expr[[1]])))

  declare_step_function_internal <- function(data) {
    if (".data" %in% arg_names) {
      mcall$expr$.data <- data
    } else if ("data" %in% arg_names) {
      mcall$expr$data <- data
    }
    lazy_eval(mcall)
  }
  attributes(declare_step_function_internal) <-
    list(call = match.call(), type = "declare_step")

  return(declare_step_function_internal)

}


