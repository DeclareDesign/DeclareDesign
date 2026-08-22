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

#' The kind of value a parameter holds
#'
#' Reported alongside each name so that a caller building a control for a
#' parameter, or deciding how to pass a replacement, does not have to inspect
#' the value itself. The kinds line up with how [redesign()] reads a bare
#' replacement: `scalar`, `vector` and `list` supply one design per element,
#' and `data`, `function` and `other` are single values.
#'
#' @keywords internal
#' @noRd
param_kind <- function(x) {
  if (is.function(x)) return("function")
  if (is.data.frame(x) || !is.null(dim(x))) return("data")
  if (is.object(x)) return("other")
  if (is.list(x)) return("list")
  if (is.atomic(x)) return(if (length(x) == 1L) "scalar" else "vector")
  "other"
}

#' Every quosure a step carries
#'
#' @keywords internal
#' @noRd
step_quosures <- function(step) {
  quos <- c(as.list(attr(step, "dots")),
            lapply(side_quo_names(), function(nm) attr(step, nm)))
  Filter(rlang::is_quosure, quos)
}

#' Find the parameters and objects a design refers to
#'
#' Walks every captured expression in the design and reports the names that
#' [redesign()] can change: the parameters a [declare_parameters()] step
#' declares, and objects the design's expressions read out of the environments
#' they were written in. Symbols that resolve to a package (`rnorm`,
#' `complete_ra`) and symbols that resolve to nothing, because they name a
#' column supplied by an earlier step, are both left out.
#'
#' An argument written as a literal is not one of them. `declare_model(N = 500)`
#' puts 500 in the design; nothing outside it holds that number and nothing
#' names it, so there is nothing for a redesign to change. `declare_parameters(
#' n = 500) + declare_model(N = n)` is how a design says a redesign may set
#' that number.
#'
#' A name a previous expression put in the data is a column, not a parameter:
#' the data mask shadows the environment, so once a step has declared `Y`, a
#' later `mean(Y)` reads the column and not whatever `Y` happens to be bound
#' to in the workspace. Only steps that build data shadow names this way. A
#' name handed to a handler (`declare_inquiry(handler = f, m_arms = m_arms)`)
#' is an argument and creates no column, so it stays visible to every later
#' step, and `redesign(design, m_arms = 4)` reaches all of them.
#'
#' `N` is its own case. Within the step that declares it, `rnorm(N)` reads the
#' number of rows fabricate is building rather than the workspace's `N`, so the
#' step shadows it. It is not a column, though, so it does not shadow anything
#' later: an estimator whose `term` reads `N`, or a second level declaring
#' `nest_level(N = N)`, is reading the workspace and can be redesigned.
#'
#' @param design A `design` or a `design_step`.
#' @return A data frame with one row per name per step: `name`, `value_str`,
#'   `kind` (`scalar`, `vector`, `list`, `data`, `function` or `other`),
#'   `step`, `quosure`, and the environment the name was found in. Rows are
#'   in step order.
#' @keywords internal
#' @noRd
#' Whether a step's named arguments become columns of the data
#'
#' A step that runs `fabricate()` turns each named dot into a column, which
#' shadows the same name for every expression after it. A step with an
#' explicit handler passes its named dots to that handler as arguments, and
#' an inquiry or an estimator names inquiries and terms rather than columns.
#'
#' @keywords internal
#' @noRd
step_builds_data <- function(step) {
  is.null(attr(step, "handler_fn")) &&
    isTRUE(attr(step, "step_type") %in%
             c("model", "measurement", "assignment", "sampling"))
}

#' The user-written functions a step holds as values
#'
#' A handler, a `.method` and a `.summary` are stored as values rather than as
#' expressions, so what they read out of their closures is invisible to a walk
#' over the design's quosures. It is not invisible to `redesign()`: the same
#' re-homing that reaches a declared parameter inside one reaches an undeclared
#' one, and DeclareDesign 1.1.1 reaches it too, by cloning the handler's whole
#' environment. Package functions are left out, since `lm_robust()`'s namespace
#' is not a design's parameters.
#'
#' @keywords internal
#' @noRd
step_user_functions <- function(step) {
  fns <- lapply(c("handler_fn", "method_arg", "summary_arg"),
                function(nm) attr(step, nm))
  Filter(function(f) is.function(f) && !fn_is_from_package(f), fns)
}

#' Was this function written by a package rather than by the user?
#'
#' Two cases, and they need different tests. A function a package *exports*
#' (`lm_robust`) has a namespace for its environment. A closure a package
#' *returns* (`label_estimator()`'s) has an ordinary function frame, and no
#' property of that frame separates it from one the user wrote at the console,
#' so it carries a mark instead. Walking into the second kind reported this
#' package's own internals (`term`, `label`, `.method`, `summary_fn`) as
#' parameters of 37 designs in the library.
#'
#' `topenv()` looks like the answer to the second case and is not: a function
#' the user defines inside a `testthat` block, or inside any package, has a
#' namespace for its `topenv()` too, so that test quietly switches the whole
#' mechanism off wherever `R CMD check` runs it.
#'
#' @keywords internal
#' @noRd
fn_is_from_package <- function(fn) {
  if (!is.function(fn)) return(TRUE)
  if (isTRUE(attr(fn, "dd_internal"))) return(TRUE)
  env <- environment(fn)
  if (!rlang::is_environment(env)) return(TRUE)
  is_package_env(env)
}

#' The names a user-written function reads out of its closure
#'
#' Its own formals are not among them, and neither is anything it assigns to
#' before reading, since a local binding shadows the closure either way. A name
#' that resolves to a package is not a parameter.
#'
#' @keywords internal
#' @noRd
closure_symbols <- function(fn) {
  body_expr <- body(fn)
  if (is.null(body_expr)) return(character(0))
  setdiff(unique(expr_symbols(body_expr)), names(formals(fn)))
}

#' Is this name bound nowhere the declaration can see, packages included?
#'
#' Distinguishes the two cases [user_binding_env()] collapses into `NULL`. A
#' name that resolves to a package (`rnorm`, `complete_ra`) is not a parameter
#' and never will be. A name that resolves to nothing is usually a column an
#' earlier step created, but it is also how a design written for a designer
#' function reads: `declare_model(N = N)` at top level has no `N` anywhere, and
#' `redesign()` is what supplies it.
#'
#' @keywords internal
#' @noRd
name_is_unbound <- function(env, name) {
  if (!rlang::is_environment(env)) return(TRUE)
  !exists(name, envir = env, inherits = TRUE)
}

#' @param include_unbound Whether to report names that are bound nowhere.
#'   `design_parameters()` leaves them out, because most of them are columns.
#'   [redesign()] needs them, because the rest are names it is expected to
#'   supply, and refusing one of those would break every design written in the
#'   `declare_model(N = N)` form a designer function is called with.
#' @keywords internal
#' @noRd
find_all_objects <- function(design, include_unbound = FALSE) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  steps <- unclass(design)
  rows <- list()
  mask <- character(0)
  declared <- declared_param_names(design)
  # A note is not something a redesign can set, so it is left out. Without this
  # a note name that happens to be bound in the workspace as well would be
  # reported as something `redesign()` could change, which it cannot.
  notes <- declared_note_names(design)
  add_row <- function(name, value, step, quosure, env) {
    rows[[length(rows) + 1L]] <<- data.frame(
      name = name, value_str = describe_value(value), kind = param_kind(value),
      declared = name %in% declared,
      step = step, quosure = quosure, env = I(list(env)),
      stringsAsFactors = FALSE
    )
  }
  # A name a design reads which turns out to be a function brings its own
  # names with it: `declare_measurement(handler = hdl)` where `hdl` reads `b`
  # makes `b` a parameter of the design, and DeclareDesign 1.1.1 treats it as
  # one. Bounded, because a function that reads a function that reads a
  # function is already past the point of being anyone's design parameter.
  add_fn_symbols <- function(fn, step, label, depth = 0L) {
    if (depth >= 3L || fn_is_from_package(fn)) return(invisible(NULL))
    env <- environment(fn)
    for (name in setdiff(closure_symbols(fn), notes)) {
      found <- user_binding_env(env, name)
      if (is.null(found)) next
      value <- tryCatch(rlang::env_get(found, name), error = function(e) NULL)
      if (inherits(value, "design")) next
      add_row(name, value, step, label, found)
      if (is.function(value)) add_fn_symbols(value, step, label, depth + 1L)
    }
    invisible(NULL)
  }
  add_quosure <- function(quo, step, masked = character(0)) {
    env <- rlang::quo_get_env(quo)
    label <- rlang::as_label(rlang::quo_get_expr(quo))
    symbols <- setdiff(unique(expr_symbols(rlang::quo_get_expr(quo))), masked)
    for (name in setdiff(symbols, notes)) {
      found <- user_binding_env(env, name)
      if (is.null(found)) {
        if (!include_unbound || !name_is_unbound(env, name)) next
        add_row(name, NULL, step, label, NULL)
        next
      }
      value <- tryCatch(rlang::env_get(found, name), error = function(e) NULL)
      if (inherits(value, "design")) next
      add_row(name, value, step, label, found)
      if (is.function(value)) add_fn_symbols(value, step, label)
    }
  }
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    dots <- attr(step, "dots")
    dot_names <- names(dots) %||% rep("", length(dots))
    # A declared parameter is reported by its evaluated value, whatever
    # expression produced it, so `ks = seq_len(m_arms)[-1]` is listed as the
    # vector it is rather than left out for not being a literal.
    if (is_parameters_step(step)) {
      values <- tryCatch(parameter_values(step), error = function(e) NULL)
      for (nm in names(dots)) {
        add_row(nm, if (nm %in% names(values)) values[[nm]] else NULL,
                i, nm, NULL)
      }
      next
    }
    # A note declaration's own names are not reported; the expressions it is
    # computed from are walked like any other, so the parameters behind a note
    # still appear.
    if (is_notes_step(step)) {
      for (j in seq_along(dots)) add_quosure(dots[[j]], i, mask)
      next
    }
    builds_data <- step_builds_data(step)
    step_mask <- character(0)
    for (j in seq_along(dots)) {
      add_quosure(dots[[j]], i, c(mask, step_mask))
      if (builds_data && nzchar(dot_names[j])) step_mask <- c(step_mask, dot_names[j])
    }
    # `N` is shadowed from the step that declares it onward, because `rnorm(N)`
    # reads the number of rows fabricate is building rather than anything
    # defined outside the design, in that step and in every later one. A
    # designer's `declare_model(N = N)` still reports `N`, from its own
    # expression, before the shadow goes up.
    mask <- c(mask, step_mask)
    # A name a handler, a `.method` or a `.summary` reads out of its closure is
    # a parameter of the design like any other. Nothing masks these: a closure
    # is evaluated in its own environment, not in the data mask, so a column of
    # the same name never shadows what it reads.
    for (fn in step_user_functions(step)) add_fn_symbols(fn, i, "closure")
    for (nm in side_quo_names()) {
      quo <- attr(step, nm)
      if (!rlang::is_quosure(quo)) next
      # `filter` and `subset` are evaluated against the data; `term` and
      # `inquiry` are evaluated in the environment they were written in, so
      # no column can shadow them.
      add_quosure(quo, i, if (nm %in% c("filter_quo", "subset_quo")) mask else character(0))
    }
  }
  out <- if (length(rows) == 0) {
    data.frame(name = character(0), value_str = character(0),
               kind = character(0), declared = logical(0), step = integer(0),
               quosure = character(0), env = I(list()),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
  row.names(out) <- NULL
  class(out) <- c("objects", "data.frame")
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
    if (is_parameters_step(step)) {
      values <- tryCatch(parameter_values(step), error = function(e) NULL)
      if (name %in% names(values)) return(values[[name]])
      next
    }
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
  # A parameter a handler or a `.method` reads out of its closure is in the
  # objects table and in none of the places walked above, so the table is the
  # fallback. Without it `check_param_vectors()` cannot tell whether such a
  # parameter already holds a vector, and the library sweep cannot test one.
  objs <- find_all_objects(design)
  idx <- match(name, objs$name)
  if (!is.na(idx) && rlang::is_environment(objs$env[[idx]])) {
    return(tryCatch(rlang::env_get(objs$env[[idx]], name), error = function(e) NULL))
  }
  NULL
}

#' Print the objects table, one row per name
#'
#' The table carries an `env` column of environments, which `print.data.frame`
#' cannot format, so the table is aggregated down to the three columns a reader
#' wants before it is printed.
#'
#' @param x An `objects` table, as returned by `find_all_objects()`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print objects
print.objects <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("No parameters or objects found in the design.\n")
    return(invisible(x))
  }
  tmp <- unique(x[c("name", "value_str", "kind", "declared", "step")])
  class(tmp) <- "data.frame"
  out <- stats::aggregate(
    step ~ name + value_str + kind + declared, data = tmp,
    FUN = function(s) paste(sort(unique(s)), collapse = ", ")
  )
  names(out)[names(out) == "step"] <- "steps"
  print(out[order(tolower(out$name)), , drop = FALSE], row.names = FALSE)
  invisible(x)
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
