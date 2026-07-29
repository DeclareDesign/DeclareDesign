#' Collect the free symbols in an expression
#'
#' Call heads count, so `g(x)` reports `g`: a design can be parameterized by
#' which function it calls. The right-hand side of `$` and `@` is a field
#' name, not a symbol that could be rebound, so only the left side is walked.
#'
#' @keywords internal
#' @noRd
expr_symbols <- function(expr) {
  if (rlang::is_symbol(expr)) return(rlang::as_string(expr))
  if (!rlang::is_call(expr)) return(character(0))
  if (rlang::is_call(expr, c("$", "@")) && length(expr) == 3L) {
    return(expr_symbols(expr[[2]]))
  }
  unlist(lapply(as.list(expr), expr_symbols), use.names = FALSE) %||% character(0)
}

#' Is this environment supplied by a package rather than by the user?
#'
#' @keywords internal
#' @noRd
is_package_env <- function(env) {
  isNamespace(env) ||
    identical(env, baseenv()) ||
    startsWith(environmentName(env), "package:")
}

#' Find the environment a name is bound in, stopping at the package envs
#'
#' Returns `NULL` when the name is unbound, or when it resolves to something a
#' package supplies (`rnorm`, `mean`, `complete_ra`), which is not a parameter
#' of the design. Names that resolve to nothing are data-mask columns.
#'
#' @keywords internal
#' @noRd
user_binding_env <- function(env, name) {
  while (rlang::is_environment(env) && !identical(env, rlang::empty_env())) {
    if (is_package_env(env)) return(NULL)
    if (rlang::env_has(env, name)) return(env)
    env <- rlang::env_parent(env)
  }
  NULL
}

#' One-line description of a value, for the objects table
#'
#' @keywords internal
#' @noRd
describe_value <- function(x) {
  if (is.function(x)) return("function")
  if (is.atomic(x) && length(x) <= 5) return(paste(deparse(x), collapse = ""))
  paste0("<", class(x)[1], ">")
}

#' Every quosure a step carries
#'
#' @keywords internal
#' @noRd
step_quosures <- function(step) {
  quos <- c(as.list(attr(step, "dots")),
            list(attr(step, "filter_quo"), attr(step, "subset_quo")))
  Filter(rlang::is_quosure, quos)
}

#' Find the parameters and objects a design refers to
#'
#' Walks every captured expression in the design and reports the names that
#' [redesign()] can change: objects the design's expressions read out of the
#' environments they were written in, and arguments declared as literal values
#' (the `N` of `declare_model(N = 500)`). Symbols that resolve to a package
#' (`rnorm`, `complete_ra`) and symbols that resolve to nothing, because they
#' name a column supplied by an earlier step, are both left out.
#'
#' A name a previous expression put in the data is a column, not a parameter:
#' the data mask shadows the environment, so once `N` has been declared by a
#' step, a later `rnorm(N)` reads the column and not whatever `N` happens to
#' be bound to in the workspace.
#'
#' @param design A `design` or a `design_step`.
#' @return A data frame with one row per name per step: `name`, `value_str`,
#'   `step`, `quosure`, and the environment the name was found in. Rows are
#'   in step order.
#' @keywords internal
#' @noRd
find_all_objects <- function(design) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  steps <- unclass(design)
  rows <- list()
  mask <- character(0)
  add_row <- function(name, value_str, step, quosure, env) {
    rows[[length(rows) + 1L]] <<- data.frame(
      name = name, value_str = value_str, step = step, quosure = quosure,
      env = I(list(env)), stringsAsFactors = FALSE
    )
  }
  add_quosure <- function(quo, step) {
    env <- rlang::quo_get_env(quo)
    label <- rlang::as_label(rlang::quo_get_expr(quo))
    for (name in setdiff(unique(expr_symbols(rlang::quo_get_expr(quo))), mask)) {
      found <- user_binding_env(env, name)
      if (is.null(found)) next
      value <- tryCatch(rlang::env_get(found, name), error = function(e) NULL)
      if (inherits(value, "design")) next
      add_row(name, describe_value(value), step, label, found)
    }
  }
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    dots <- attr(step, "dots")
    dot_names <- names(dots) %||% rep("", length(dots))
    for (j in seq_along(dots)) {
      expr <- rlang::quo_get_expr(dots[[j]])
      if (nzchar(dot_names[j]) && is.atomic(expr) && length(expr) <= 5) {
        add_row(dot_names[j], describe_value(expr), i, dot_names[j], NULL)
      }
      add_quosure(dots[[j]], i)
      if (nzchar(dot_names[j])) mask <- c(mask, dot_names[j])
    }
    for (quo in Filter(rlang::is_quosure,
                       list(attr(step, "filter_quo"), attr(step, "subset_quo")))) {
      add_quosure(quo, i)
    }
  }
  out <- if (length(rows) == 0) {
    data.frame(name = character(0), value_str = character(0),
               step = integer(0), quosure = character(0),
               env = I(list()), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
  row.names(out) <- NULL
  out
}

#' The value a design's parameter currently holds
#'
#' Looks for `name` the way [redesign()] rebinds it: as a literal argument
#' first, then as a name read out of the environment an expression was
#' captured in. Returns `NULL` when the design does not use the name, or when
#' the value cannot be resolved.
#'
#' @keywords internal
#' @noRd
current_param_value <- function(design, name) {
  for (step in unclass(design)) {
    dots <- attr(step, "dots")
    idx <- match(name, names(dots) %||% character(0))
    if (!is.na(idx)) {
      expr <- rlang::quo_get_expr(dots[[idx]])
      if (is.atomic(expr)) return(expr)
    }
    for (quo in step_quosures(step)) {
      if (!name %in% expr_symbols(rlang::quo_get_expr(quo))) next
      found <- user_binding_env(rlang::quo_get_env(quo), name)
      if (is.null(found)) next
      return(tryCatch(rlang::env_get(found, name), error = function(e) NULL))
    }
  }
  NULL
}

#' Aggregate the objects table to one row per name and print it
#'
#' @keywords internal
#' @noRd
print_objects <- function(objects) {
  if (nrow(objects) == 0) {
    cat("No parameters or objects found in the design.\n")
    return(invisible(objects))
  }
  tmp <- unique(objects[c("name", "value_str", "step")])
  out <- stats::aggregate(
    step ~ name + value_str, data = tmp,
    FUN = function(s) paste(sort(unique(s)), collapse = ", ")
  )
  names(out)[names(out) == "step"] <- "steps"
  print(out[order(tolower(out$name)), , drop = FALSE], row.names = FALSE)
  invisible(objects)
}
