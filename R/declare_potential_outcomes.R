
## this function is from lazyeval version git version, commit c155c3d
freeze_environment <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

#' @export
declare_potential_outcomes <- function(..., potential_outcomes_function = potential_outcomes_function_default) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(potential_outcomes_function)
  if (!("data" %in% names(formals(func)))) {
    stop("Please choose potential_outcomes_function with a data argument.")
  }
  potential_outcomes_function_internal <- function(data) {
    args$data <- data
    do.call(func, args = args, envir = env)
  }
  attributes(potential_outcomes_function_internal) <-
    list(call = match.call(), type = "potential_outcomes")

  return(potential_outcomes_function_internal)
}


#' @export
potential_outcomes_function_default <-
  function(data,
           assignment_variable_name = "Z",
           condition_names = c(0, 1),
           ...) {
    options <- eval(substitute(alist(...)))

    if ("formula" %in% names(options)) {
      # TODO: checks re: condition names and assignment variable names
      potential_outcomes_function <-
        potential_outcomes_function_formula(
          data = data,
          assignment_variable_name = assignment_variable_name,
          condition_names = condition_names,
          ... = ...
        )
    } else{
      potential_outcomes_function <-
        potential_outcomes_function_discrete(data = data, ... = ...)
    }

    return(potential_outcomes_function)

  }

potential_outcomes_function_formula <-
  function(data,
           formula,
           condition_names,
           assignment_variable_name) {
    outcome_variable_name <- as.character(formula[[2]])

    for (cond in condition_names) {
      ## make a dataset that we manipulate to make PO columns
      ## by adding Z variables that are all 0's or all 1's for example
      data_environment <- list2env(data)
      data_environment$N <- nrow(data)

      assign(x = assignment_variable_name,
             value = rep(cond, nrow(data)),
             envir = data_environment)

      data[, paste0(c(outcome_variable_name, assignment_variable_name, cond),
                    collapse = "_")] <-
        eval(expr = formula[[3]], envir = data_environment)
    }

    return(data)
  }


#' @importFrom fabricatr fabricate_data
potential_outcomes_function_discrete <- function(data, ...) {
  options <- eval(substitute(alist(...)))

  potential_outcomes_function <- fabricate_data

  argument_names_potential_outcomes_function <-
    names(formals(potential_outcomes_function))

  variable_names <- sapply(
    names(options),
    FUN =
      function(x)
        paste(strsplit(x, split = "_")[[1]][1], strsplit(x, split = "_")[[1]][2], sep = "_")
  )

  if (!all(variable_names == variable_names[1])) {
    stop("All of the variable names you create should begin with the same prefix, i.e. Y_Z_")
  }

  outcome_variable_name <-
    strsplit(variable_names[1], split = "_")[[1]][1]

  options$data <- data

  return(do.call(potential_outcomes_function, args = options))
}
