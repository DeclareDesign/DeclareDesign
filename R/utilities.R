## this function is from lazyeval version git version, commit c155c3d
freeze_environment <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

from_package <- function(func, package) {
  func_package <-
    tryCatch(
      getNamespaceName(environment(func)),
      error = function(e)
        NULL
    )
  ifelse(is.null(func_package), FALSE, func_package == package)
}

#' @importFrom rlang quos lang_fn lang_modify eval_tidy
wrap_step <- function(...) {
  ## this function allows you to put any R expression
  ## such a dplyr call mutate
  ## into the causal order, i.e.
  ## declare_design(pop(), po, declare_step(mutate(q = 5)))

  step_call <- quos(...)[[1]]

  arg_names <- names(formals(lang_fn(step_call)))

  declare_step_function_internal <- function(data) {
    if (".data" %in% arg_names) {
      step_call <- lang_modify(step_call, .data = data)
    } else if ("data" %in% arg_names) {
      step_call <- lang_modify(step_call, data = data)
    }
    eval_tidy(step_call)
  }

  attributes(declare_step_function_internal) <-
    list(call = match.call(), type = "declare_step")

  return(declare_step_function_internal)

}
