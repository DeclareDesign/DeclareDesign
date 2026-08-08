#' Collect the free symbols in an expression
#'
#' Call heads count, so `g(x)` reports `g`: a design can be parameterized by
#' which function it calls. The right-hand side of `$` and `@` is a field
#' name, not a symbol that could be rebound, so only the left side is walked.
#'
#' A missing argument, the blank in `x[, 1]`, is a symbol whose name is the
#' empty string. It names nothing, and asking an environment whether it holds
#' the empty name is an error, so it is dropped here.
#'
#' @keywords internal
#' @noRd
expr_symbols <- function(expr) {
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_string(expr)
    return(if (nzchar(name)) name else character(0))
  }
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

#' Sentinel distinguishing "bound to NULL" from "not bound"
#' @keywords internal
#' @noRd
capture_absent <- local(structure(list(), class = "dd_capture_absent"))

#' Collect the workspace objects a set of expressions reads
#'
#' Walks `names`, and for each one bound in `globalenv()` copies the value into
#' `target`. A user-written function is re-homed onto a child of `target` and
#' its body walked in turn, so a helper's own dependencies travel with it.
#'
#' `registry` holds every name already handled, keyed once *before* the
#' recursion into a function body, which is what makes a self-recursive helper
#' (`f <- function(n) n * f(n - 1)`) and a mutually recursive pair terminate:
#' the second sighting of a name finds it in the registry and binds the
#' already-re-homed function instead of descending again.
#'
#' @keywords internal
#' @noRd
capture_names_into <- function(names, env, target, registry,
                               globals_only = FALSE) {
  for (name in unique(names)) {
    if (!nzchar(name)) next
    if (rlang::env_has(registry, name)) {
      if (!rlang::env_has(target, name)) {
        assign(name, rlang::env_get(registry, name), envir = target)
      }
      next
    }
    found <- user_binding_env(env, name)
    if (is.null(found)) next
    # When the declaration environment is retained (the dynamic-lookup
    # fallback), only globalenv() needs copying: everything else still resolves
    # through the parent, and copying it would write it to disk twice, since
    # R's serialiser does not deduplicate across a child and its parent.
    if (globals_only && !identical(found, globalenv())) next
    if (!rlang::env_has(found, name)) next
    value <- tryCatch(rlang::env_get(found, name),
                      error = function(e) capture_absent)
    if (inherits(value, "dd_capture_absent")) next
    # NULL is a value. `env_get()` returning it is not the same as finding
    # nothing, and treating the two alike drops a legitimately NULL object.
    assign(name, value, envir = registry)
    fenv <- if (is.function(value)) environment(value) else NULL
    if (is.function(value) && rlang::is_environment(fenv) && !is_package_env(fenv)) {
      inner <- new.env(parent = target)
      environment(value) <- inner
      assign(name, value, envir = registry)
      assign(name, value, envir = target)
      capture_names_into(
        setdiff(expr_symbols(body(value)), names(formals(value))),
        fenv, inner, registry, globals_only
      )
      # A recursive helper has to be able to find itself.
      if (!rlang::env_has(inner, name)) assign(name, value, envir = inner)
      next
    }
    assign(name, value, envir = target)
  }
}

#' Calls that resolve a name at run time rather than naming it in the code
#'
#' `expr_symbols()` reads names out of the expression tree. These functions
#' take the name as data instead, so nothing in the tree records what will be
#' looked up, and a pruned environment would not contain it.
#'
#' @keywords internal
#' @noRd
dynamic_lookup_calls <- c(
  "get", "get0", "mget", "exists", "assign", "do.call", "eval", "evalq",
  "eval.parent", "parse", "str2lang", "str2expression", "match.fun",
  "Recall", "sys.function", "parent.frame", "environment", "as.name",
  "as.symbol", "eval_tidy", "sym", "syms", "data_sym", "data_syms"
)

#' Would pruning lose something this declaration needs?
#'
#' True when any expression, or the body of any user-written function it
#' reaches, calls something from [dynamic_lookup_calls]. Deliberately
#' conservative: a false positive costs a design that carries more than it
#' needs, a false negative costs a design that does not run.
#'
#' @keywords internal
#' @noRd
declaration_uses_dynamic_lookup <- function(exprs, env) {
  seen <- new.env(parent = emptyenv())
  pending <- exprs
  while (length(pending)) {
    expr <- pending[[1L]]
    pending <- pending[-1L]
    syms <- expr_symbols(expr)
    if (any(syms %in% dynamic_lookup_calls)) return(TRUE)
    for (name in unique(syms)) {
      if (!nzchar(name) || rlang::env_has(seen, name)) next
      assign(name, TRUE, envir = seen)
      found <- user_binding_env(env, name)
      if (is.null(found) || !rlang::env_has(found, name)) next
      value <- tryCatch(rlang::env_get(found, name), error = function(e) NULL)
      if (!is.function(value)) next
      fenv <- environment(value)
      if (!rlang::is_environment(fenv) || is_package_env(fenv)) next
      pending <- c(pending, list(body(value)))
    }
  }
  FALSE
}

#' Give a set of quosures an environment holding what they read
#'
#' A quosure captured at the console keeps `globalenv()` as its environment,
#' and R restores `globalenv()` by reference rather than serialising it. A
#' design declared at top level therefore loses its parameters the moment it
#' crosses a process boundary: `saveRDS()` then `readRDS()` in a fresh session,
#' or `simulate_design()` under `future::plan(multisession)`. It also tracks
#' later edits to those bindings, so a design silently changes meaning when an
#' unrelated line runs in the console.
#'
#' `user_binding_env()` already draws the line in the right place: it returns
#' `NULL` for a name a package supplies and for a name that resolves to
#' nothing, which is a column an earlier step created. Neither is copied, which
#' is why this needs none of the exclusion lists that
#' `DeclareDesign:::capture_globals_quosure()` carries.
#'
#' One environment is built per declaration rather than one per quosure,
#' because [dots_env()] hands the *first* dot's environment to the step's
#' executor. Capturing per quosure leaves an estimator whose first argument is
#' a formula evaluating its remaining arguments in an environment that captured
#' nothing.
#'
#' @param dots A list of quosures.
#' @return The list, re-environed where anything was found.
#' @keywords internal
#' @noRd
capture_dots_env <- function(dots) {
  if (!length(dots)) return(dots)
  envs <- lapply(dots, function(q) {
    if (rlang::is_quosure(q)) rlang::quo_get_env(q) else NULL
  })
  out <- dots
  handled <- logical(length(dots))
  for (i in seq_along(dots)) {
    if (handled[[i]]) next
    env <- envs[[i]]
    if (!rlang::is_environment(env) || is_package_env(env)) next
    # Every quosure captured in the same environment shares one captured
    # environment, so the step's executor sees the same bindings whichever
    # dot dots_env() happens to pick.
    same <- vapply(envs, function(e) identical(e, env), logical(1))
    exprs <- lapply(dots[same], rlang::quo_get_expr)
    names_needed <- unlist(lapply(exprs, expr_symbols), use.names = FALSE)
    handled[same] <- TRUE
    if (!length(names_needed)) next
    # A declaration that looks names up at run time cannot be pruned: nothing
    # in its expression tree says what it will need. Fall back to retaining the
    # declaration environment for that declaration alone, which is what the
    # rest of the library used to do for all of them.
    dynamic <- declaration_uses_dynamic_lookup(exprs, env)
    captured <- new.env(parent = if (dynamic) env else globalenv())
    capture_names_into(names_needed, env, captured,
                       new.env(parent = emptyenv()), globals_only = dynamic)
    # Pruning detaches unconditionally: leaving the original environment in
    # place when nothing was captured would let a declaration that reads only
    # columns and package functions drag its whole scope along anyway. The
    # fallback has nothing to gain from re-environing when it captured nothing.
    if (dynamic && !length(ls(captured, all.names = TRUE))) next
    for (j in which(same)) {
      out[[j]] <- rlang::new_quosure(rlang::quo_get_expr(dots[[j]]), captured)
    }
  }
  names(out) <- names(dots)
  if (inherits(dots, "quosures")) out <- rlang::as_quosures(out)
  out
}

#' Single-quosure form of [capture_dots_env()], for `filter` and `subset`
#' @keywords internal
#' @noRd
capture_quosure_env <- function(quo) {
  if (!rlang::is_quosure(quo)) return(quo)
  capture_dots_env(list(quo))[[1L]]
}
