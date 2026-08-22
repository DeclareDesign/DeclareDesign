#' Rebuild a step's closure after parameter rebinding
#'
#' Reconstructs the execution function so the new dots take effect.
#'
#' @keywords internal
#' @noRd
rebuild_step <- function(step, new_dots, new_side = list()) {
  step_type   <- attr(step, "step_type")
  causal_type <- attr(step, "causal_type")
  label       <- attr(step, "label")
  call        <- attr(step, "call")
  handler_expr <- attr(step, "handler_expr")
  for (nm in side_quo_names()) {
    if (!nm %in% names(new_side)) new_side[[nm]] <- attr(step, nm)
  }
  new_fn <- switch(
    step_type,
    "model"       = make_fabricate_step(new_dots, id_label_na = FALSE),
    "measurement" = make_fabricate_step(new_dots, id_label_na = TRUE),
    "assignment"  = make_fabricate_step(new_dots, id_label_na = TRUE),
    "sampling"    = make_sampling_step(new_dots, new_side$filter_quo),
    "inquiry"     = make_inquiry_step(new_dots, new_side$subset_quo, label,
                                       handler = attr(step, "handler_fn")),
    "estimator"   = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = new_side$inquiry_quo,
      term        = new_side$term_quo,
      add_inquiry = TRUE,
      handler     = attr(step, "handler_fn")
    ),
    "test"        = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = NULL,
      term        = new_side$term_quo,
      add_inquiry = FALSE,
      handler     = attr(step, "handler_fn")
    ),
    "diagnosand"  = {
      d <- new_dots
      function(data) {
        out <- purrr::imap(d, function(q, nm) {
          val <- rlang::eval_tidy(q, data = as.list(data))
          tibble::tibble(diagnosand = nm, value = val)
        })
        dplyr::bind_rows(out)
      }
    },
    "custom"      = {
      # Must mirror declare_step()'s own closure exactly. Evaluating the dots
      # here instead of passing them as written broke every tidyselect handler
      # the moment a design was redesigned: `id_cols = pair` reached
      # pivot_wider() as the column's contents rather than as the name `pair`.
      handler  <- attr(step, "handler_fn")
      decl_env <- dots_env(new_dots, default = globalenv())
      args     <- dots_as_written(new_dots)
      d        <- new_dots
      function(data) {
        if (handler_is_fabricate(handler)) {
          rlang::inject(handler(data = data, !!!d))
        } else {
          call_env <- rlang::env(decl_env, .dd_data = data)
          eval(rlang::call2(handler, quote(.dd_data), !!!args), envir = call_env)
        }
      }
    },
    step
  )
  out <- build_step(
    fn           = new_fn,
    handler_expr = handler_expr,
    dots         = new_dots,
    step_type    = step_type,
    causal_type  = causal_type,
    label        = label,
    call         = call
  )
  carry <- c("method_arg", "summary_arg", "handler_fn", "draws", "method_name")
  for (nm in carry) {
    if (!is.null(attr(step, nm))) attr(out, nm) <- attr(step, nm)
  }
  for (nm in side_quo_names()) {
    if (!is.null(new_side[[nm]])) attr(out, nm) <- new_side[[nm]]
  }
  out
}

#' The declared arguments a step holds as quosures rather than as values
#'
#' These are rebound by [redesign()] and read by `find_all_objects()` exactly
#' as the dots are. Anything not on this list is frozen when the step is
#' written, and so is out of reach of a redesign.
#'
#' @keywords internal
#' @noRd
side_quo_names <- function() {
  c("filter_quo", "subset_quo", "term_quo", "inquiry_quo")
}

#' Does this body reassign the name upward with `<<-`?
#'
#' The one case where re-homing a function changes what it means. `<<-` starts
#' its search in the function's enclosing environment, so putting a new
#' environment there sends the assignment to the copy instead of to the binding
#' the author meant. Such a function is left alone.
#'
#' @keywords internal
#' @noRd
superassigns_name <- function(expr, name) {
  if (!is.call(expr)) return(FALSE)
  if (identical(expr[[1]], quote(`<<-`)) && length(expr) >= 2L &&
      identical(expr[[2]], as.name(name))) {
    return(TRUE)
  }
  parts <- as.list(expr)[-1]
  for (part in parts) {
    if (missing(part) || is.null(part)) next
    if (superassigns_name(part, name)) return(TRUE)
  }
  FALSE
}

#' Give a user function a parameter it reads out of its own closure
#'
#' A handler, a `.method`, a `.summary`, or any function the design reads by
#' name, is stored as a value, so no amount of expression rebinding reaches
#' what it reads. Re-homing it does, and DeclareDesign 1.1.1 does the same
#' thing more bluntly: it copies the whole enclosing environment and rebuilds
#' the function in the copy. This puts a child environment holding the one
#' parameter in front of the original instead, which leaves every other name
#' the function reads exactly where it was.
#'
#' Recurses one function deep at a time to a bounded depth, so a handler that
#' calls a helper that reads the parameter is reached the way `hdl` calling `f`
#' is. Returns `NULL` when nothing in reach reads the name.
#'
#' @keywords internal
#' @noRd
rehome_fn_deep <- function(fn, name, new_val, depth = 0L) {
  if (depth >= 3L || !is.function(fn) || fn_is_from_package(fn)) return(NULL)
  env <- environment(fn)
  if (name %in% closure_symbols(fn)) {
    if (superassigns_name(body(fn), name)) return(NULL)
    return(rehome_fn_on_params(fn, stats::setNames(list(new_val), name)))
  }
  inner <- list()
  for (sym in closure_symbols(fn)) {
    found <- user_binding_env(env, sym)
    if (is.null(found)) next
    val <- tryCatch(rlang::env_get(found, sym), error = function(e) NULL)
    if (!is.function(val)) next
    rehomed <- rehome_fn_deep(val, name, new_val, depth + 1L)
    if (!is.null(rehomed)) inner[[sym]] <- rehomed
  }
  if (!length(inner)) return(NULL)
  new_env <- rlang::new_environment(data = inner, parent = env)
  attr(new_env, "dd_param_env") <- TRUE
  environment(fn) <- new_env
  fn
}

#' Rebind the functions a quosure reads, when they are what hold the parameter
#'
#' `declare_measurement(handler = hdl)` keeps `hdl` as an expression, so the
#' quosure reads a name and the name resolves to a function. Rebinding the
#' parameter in that quosure reaches nothing, because the parameter is inside
#' `hdl`. Rebinding `hdl` itself, to a copy that can see the new value, does.
#'
#' @keywords internal
#' @noRd
rehome_quo_functions <- function(quo, name, new_val) {
  if (!rlang::is_quosure(quo)) return(NULL)
  env <- rlang::quo_get_env(quo)
  if (!rlang::is_environment(env)) return(NULL)
  swapped <- list()
  for (sym in unique(expr_symbols(rlang::quo_get_expr(quo)))) {
    found <- user_binding_env(env, sym)
    if (is.null(found)) next
    val <- tryCatch(rlang::env_get(found, sym), error = function(e) NULL)
    if (!is.function(val)) next
    rehomed <- rehome_fn_deep(val, name, new_val)
    if (!is.null(rehomed)) swapped[[sym]] <- rehomed
  }
  if (!length(swapped)) return(NULL)
  new_env <- rlang::env_clone(env)
  rlang::env_bind(new_env, !!!swapped)
  rlang::new_quosure(rlang::quo_get_expr(quo), env = new_env)
}

#' Rebind a parameter in a quosure, by name or through the functions it reads
#'
#' @keywords internal
#' @noRd
rebind_quo_any <- function(quo, name, new_val) {
  out <- rebind_quo_param(quo, name, new_val)
  through <- rehome_quo_functions(out %||% quo, name, new_val)
  through %||% out
}

#' Rebind one parameter inside a single quosure
#'
#' Returns `NULL` when the quosure does not read the parameter, so the caller
#' can tell "unchanged" from "changed".
#'
#' @keywords internal
#' @noRd
rebind_quo_param <- function(quo, param_name, new_val) {
  if (!rlang::is_quosure(quo) || !quo_uses_param(quo, param_name)) return(NULL)
  new_env <- rlang::env_clone(rlang::quo_get_env(quo))
  rlang::env_bind(new_env, !!param_name := new_val)
  rlang::new_quosure(rlang::quo_get_expr(quo), env = new_env)
}

#' Rebind parameters in a single design
#'
#' @keywords internal
#' @noRd
modify_design_params <- function(design, params) {
  # A name a `declare_parameters()` step declares is changed at that step and
  # nowhere else; `construct_design()` then pushes the new value onto every
  # step that reads it. Rebinding it step by step as well is what would let a
  # redesign reach a column that happens to share the parameter's name.
  declared <- declared_param_names(design)
  # A note is never a redesign target; `check_params_in_design()` has already
  # refused one by name, and this keeps the rebinding out of the note's own
  # declaration so a parameter behind it is reached there instead.
  params <- params[setdiff(names(params), declared_note_names(design))]
  new_steps <- lapply(unclass(design), function(step) {
    own <- if (is_parameters_step(step)) {
      names(attr(step, "dots")) %||% character(0)
    } else {
      character(0)
    }
    params <- params[setdiff(names(params), setdiff(declared, own))]
    if (length(params) == 0) return(step)
    declares_own <- length(own) > 0 && !is.null(names(attr(step, "dots")))
    dots <- attr(step, "dots")
    side <- lapply(stats::setNames(side_quo_names(), side_quo_names()),
                   function(nm) attr(step, nm))
    # A step with no dots and no side quosures can still hold the parameter:
    # `declare_step(handler = f)` keeps `f` as a value, and what `f` reads out
    # of its closure is reached by re-homing it rather than by rebinding an
    # expression. Returning early here skipped exactly that case.
    holds_fn <- any(vapply(c("handler_fn", "method_arg", "summary_arg"),
                           function(nm) is.function(attr(step, nm)), logical(1)))
    if ((is.null(dots) || length(dots) == 0) &&
        all(vapply(side, is.null, logical(1))) && !holds_fn) {
      return(step)
    }
    new_dots <- dots
    new_side <- side
    changed <- FALSE
    for (param_name in names(params)) {
      new_val <- params[[param_name]]
      if (length(new_dots) > 0) {
        for (j in seq_along(new_dots)) {
          q <- new_dots[[j]]
          # A `declare_parameters()` step is the one place a redesign replaces
          # an argument because of its *name*: that is what declaring a
          # parameter means. Everywhere else the parameter is reached as a
          # name the expression reads, either as a free symbol or as a binding
          # in the environment the declaration was written in. Matching names
          # in an ordinary step is what used to let `redesign(sd = 3)` reach a
          # column called `sd`, and what made a literal `declare_model(N = 100)`
          # redesignable without being declared.
          if (declares_own && identical(names(new_dots)[j], param_name)) {
            new_dots[[j]] <- rlang::new_quosure(
              rlang::expr(!!new_val),
              env = rlang::quo_get_env(q)
            )
            changed <- TRUE
            next
          }
          rebound <- rebind_quo_any(q, param_name, new_val)
          if (!is.null(rebound)) {
            new_dots[[j]] <- rebound
            changed <- TRUE
          }
        }
      }
      for (nm in names(new_side)) {
        rebound <- rebind_quo_any(new_side[[nm]], param_name, new_val)
        if (!is.null(rebound)) {
          new_side[[nm]] <- rebound
          changed <- TRUE
        }
      }
      # A handler, a `.method` or a `.summary` held as a step attribute rather
      # than read from a name. `bind_params_into_step()` has always done this
      # for a declared parameter; an undeclared one needs it just as much.
      for (nm in c("handler_fn", "method_arg", "summary_arg")) {
        rehomed <- rehome_fn_deep(attr(step, nm), param_name, new_val)
        if (!is.null(rehomed)) {
          attr(step, nm) <- rehomed
          changed <- TRUE
        }
      }
    }
    if (!changed) return(step)
    # Sampling and inquiry used to be rebuilt again here, by hand, after
    # rebuild_step() had already rebuilt them. The switch covers both.
    out_step <- rebuild_step(step, new_dots, new_side)
    if (!is.null(attr(step, "draws"))) {
      attr(out_step, "draws") <- attr(step, "draws")
    }
    out_step
  })
  construct_design(setNames(new_steps, names(design)))
}

#' Test whether rebinding `name` would change what a quosure evaluates to
#'
#' True when the name is bound somewhere in the environment the quosure was
#' captured in, or appears as a free symbol in its expression. The two cases
#' are separate: a designer function's argument is in the environment but not
#' the expression, and a parameter used only inside a data-mask expression
#' (`rnorm(N)`) is in the expression but may be bound nowhere.
#'
#' [modify_design_params()] and [step_uses_param()] must agree on this, or
#' `redesign()` warns about a parameter it goes on to change (or changes one
#' it warned about).
#'
#' @keywords internal
#' @noRd
quo_uses_param <- function(quo, name) {
  env_has_var(rlang::quo_get_env(quo), name) ||
    expr_has_symbol(rlang::quo_get_expr(quo), name)
}

#' Test whether a step would respond to a change in `name`
#'
#' @keywords internal
#' @noRd
step_uses_param <- function(step, name) {
  if (name %in% names(attr(step, "dots"))) return(TRUE)
  any(vapply(step_quosures(step), quo_uses_param, logical(1), name = name))
}

#' Refuse a redesign of a name a `declare_notes()` step computes
#'
#' The most specific of the three refusals, so it runs first: a note is not
#' merely unreachable, it is a quantity the design works out, and saying so is
#' more use than saying the design writes it down.
#'
#' @keywords internal
#' @noRd
check_params_are_not_notes <- function(design, param_names) {
  notes <- intersect(param_names, declared_note_names(design))
  if (!length(notes)) return(invisible(NULL))
  stop(paste(notes, collapse = ", "),
       if (length(notes) > 1) " are notes, not parameters." else
         " is a note, not a parameter.",
       "\n",
       "A note is computed while the design runs. Change the parameters it ",
       "is computed from, or declare it with `declare_parameters()` if it ",
       "is meant to be set directly.", call. = FALSE)
}

#' Refuse a redesign of an argument the design wrote down as a literal
#'
#' `declare_model(N = 500)` puts 500 in the design. Nothing outside the design
#' holds that number and nothing names it, so a redesign has nothing to change
#' and used to be honoured by rewriting the argument because its *name*
#' matched. That is the branch that let `redesign(sd = 3)` reach a column
#' called `sd`, and it is gone.
#'
#' The message leads with the ordinary way out, which is to give the value a
#' name outside the design (`N <- 500` at the top of a script, or a designer
#' function's argument) and read it from there. `declare_parameters()` is the
#' second suggestion, not the first: most designs never need it, and it earns
#' its place when several steps read the value or when a column shares its
#' name. Both are shown with the value the argument currently holds and with
#' the `declare_*()` verb the argument actually sits in, so the advice can be
#' pasted rather than translated.
#'
#' Erring rather than warning is the point. The alternative is a design that
#' silently keeps the value it was written with, which is the failure this
#' whole line of work exists to remove.
#'
#' @keywords internal
#' @noRd
check_params_are_declared <- function(design, param_names, reachable) {
  undeclared <- setdiff(param_names, declared_param_names(design))
  if (!length(undeclared)) return(invisible(NULL))
  undeclared <- setdiff(undeclared, reachable)
  if (!length(undeclared)) return(invisible(NULL))
  literal <- character(0)
  verbs <- character(0)
  for (step in unclass(design)) {
    if (is_parameters_step(step)) next
    nms <- names(attr(step, "dots")) %||% character(0)
    for (name in intersect(setdiff(undeclared, literal), nms)) {
      literal <- c(literal, name)
      verbs <- c(verbs, step_verb(step))
    }
  }
  if (!length(literal)) return(invisible(NULL))
  one <- literal[[1]]
  verb <- verbs[[1]]
  literal <- unique(literal)
  value <- format_param_value(current_param_value(design, one))
  rlang::abort(c(
    paste0(paste(literal, collapse = ", "),
           if (length(literal) > 1) " are arguments this design writes down, not parameters."
           else " is an argument this design writes down, not a parameter."),
    "i" = paste0("To redesign over values of `", one, "`, give it a name outside ",
                 "the design: `", one, " <- ", value, "`, then `", verb, "(",
                 one, " = ", one, ", ...)`. A designer function's argument does ",
                 "the same thing."),
    "i" = paste0("`declare_parameters(", one, " = ", value, ")` names it inside ",
                 "the design instead, which is worth it when several steps read ",
                 "it or when a column shares its name.")
  ))
}

#' The declare_*() verb a step was written with
#'
#' Used to quote the user's own call back at them, so the advice on how to make
#' an argument redesignable names the step it is actually in.
#'
#' @keywords internal
#' @noRd
step_verb <- function(step) {
  call <- attr(step, "call")
  if (is.call(call) && is.name(call[[1]])) return(as.character(call[[1]]))
  paste0("declare_", attr(step, "step_type") %||% "model")
}

#' A value short enough to paste back into an error message
#'
#' @keywords internal
#' @noRd
format_param_value <- function(value) {
  if (is.null(value)) return("<value>")
  if (!is.atomic(value) || is.object(value) || length(value) > 5L) return("<value>")
  out <- paste(deparse(value), collapse = " ")
  if (nchar(out) > 40L) "<value>" else out
}

#' Warn about requested parameters no step would respond to
#'
#' @keywords internal
#' @noRd
check_params_in_design <- function(design, param_names, reachable) {
  # One reachability set for all three checks, so a name cannot be warned
  # about as absent by one and changed by another. `step_uses_param()` used to
  # answer this and could not see a parameter held inside a handler's closure.
  missing <- setdiff(param_names, reachable)
  if (length(missing) == 0) return(invisible(NULL))
  rlang::warn(paste0(
    "You requested a change to ", paste(missing, collapse = ", "),
    " but ", paste(missing, collapse = ", "),
    if (length(missing) > 1) " are" else " is", " not found in the design."
  ))
}

#' Warn when a vector-valued parameter is handed a bare vector
#'
#' An atomic vector always supplies one value per element, so
#' `redesign(design, n_units = c(50, 100))` means two designs. That rule is
#' unambiguous only until the parameter itself holds a vector: asking for
#' `prob_each = c(0, .5, .5)` then produces three designs holding one number
#' each, which is almost never what was meant and which does not fail until
#' something draws from them. Warning here puts the complaint at the call.
#'
#' @keywords internal
#' @noRd
check_param_vectors <- function(design, params) {
  for (name in names(params)) {
    supplied <- params[[name]]
    # Values `as_param_list()` keeps whole are never ambiguous: a data frame,
    # a matrix or any classed object is one replacement, not a sweep.
    if (is.object(supplied) || !is.null(dim(supplied))) next
    if (!is.atomic(supplied) || length(supplied) < 2L) next
    current <- current_param_value(design, name)
    if (!is.atomic(current) || length(current) < 2L) next
    rlang::warn(c(
      paste0("`", name, "` currently holds ", length(current), " values, so ",
             paste(deparse(supplied), collapse = " "), " is being read as ",
             length(supplied), " designs, one value each."),
      "i" = "Wrap it in `list()` to use it as a single replacement."
    ))
  }
  invisible(NULL)
}

#' Test whether an environment chain contains a binding a design could own
#'
#' Walks up to and including the global environment and stops there, so the
#' attached packages behind it do not count. Inheriting all the way to `base`
#' would mean any parameter whose name a package happens to export is treated
#' as part of the design: `redesign(design, n = 200)` on a design with no `n`
#' found `dplyr::n` and so never warned, which is the one case the warning
#' exists for.
#'
#' The other half of [quo_uses_param()], `expr_has_symbol()`, still catches a
#' name the step's expression mentions outright, so a design that really does
#' read a package object keeps working.
#'
#' @keywords internal
#' @noRd
env_has_var <- function(env, name) {
  if (!rlang::is_environment(env)) return(FALSE)
  repeat {
    found <- tryCatch(rlang::env_has(env, name, inherit = FALSE)[[1]],
                      error = function(e) FALSE)
    if (found) return(TRUE)
    if (identical(env, globalenv()) || identical(env, emptyenv())) return(FALSE)
    env <- rlang::env_parent(env)
  }
}

#' Test whether an expression mentions a symbol
#'
#' Walks the language tree of `expr` looking for the symbol `name`. Used by
#' `redesign()` to decide whether a parameter is referenced inside a captured
#' quosure even when no binding exists in the surrounding environment.
#'
#' @keywords internal
#' @noRd
expr_has_symbol <- function(expr, name) {
  if (is.symbol(expr)) return(identical(as.character(expr), name))
  if (is.call(expr)) {
    for (i in seq_along(expr)) {
      if (expr_has_symbol(expr[[i]], name)) return(TRUE)
    }
  }
  FALSE
}

#' Split a supplied parameter into the list of values it should take
#'
#' A bare atomic vector supplies one value per element and a bare list supplies
#' one value per element. Everything else is a single value: a function, a
#' formula, an environment, anything carrying a class (a data frame, a factor,
#' a fitted model), and anything with a `dim` attribute (a matrix, an array).
#'
#' A data frame is a list and a matrix is atomic, so without the first test
#' both would be taken apart: `redesign(design, pilot = df)` would ask for one
#' design per column, which is never what a data-valued parameter means. To
#' vary such a parameter across designs, pass a list of values.
#'
#' @keywords internal
#' @noRd
as_param_list <- function(v) {
  if (is.object(v) || !is.null(dim(v))) return(list(v))
  if (is.list(v)) return(v)
  if (is.atomic(v) && !is.null(v)) return(as.list(v))
  list(v)
}

#' Collapse a column of parameter values back to an atomic vector if possible
#'
#' @keywords internal
#' @noRd
simplify_param_col <- function(col) {
  scalar <- vapply(col, function(x) is.atomic(x) && length(x) == 1L, logical(1))
  if (all(scalar)) unlist(col) else col
}

#' Build the parameter grid for redesign() and expand_design()
#'
#' With `expand = TRUE` the cross-product of values is taken; with
#' `expand = FALSE` parallel vectors are zipped and length-1 entries recycled.
#' Non-atomic values (functions, formulas) survive in list columns.
#'
#' @keywords internal
#' @noRd
param_grid <- function(params, expand = TRUE) {
  if (length(params) == 0L) return(tibble::tibble())
  cols <- lapply(params, as_param_list)
  lens <- lengths(cols)
  idx <- if (expand) {
    expand.grid(lapply(lens, seq_len), KEEP.OUT.ATTRS = FALSE)
  } else {
    n <- max(lens)
    if (any(lens != 1L & lens != n)) {
      stop("All parameter vectors must have length 1 or the same length when ",
           "`.expand = FALSE`.")
    }
    as.data.frame(lapply(lens, function(l) if (l == 1L) rep(1L, n) else seq_len(l)))
  }
  out <- purrr::imap(cols, function(col, nm) simplify_param_col(col[idx[[nm]]]))
  tibble::as_tibble(out)
}

#' Re-parameterize a design
#'
#' Replaces parameter values in the captured environments of a design's steps,
#' producing one or more modified designs. With `.expand = TRUE` (the default),
#' the cross-product of parameter values is taken; with `.expand = FALSE`,
#' values are zipped position-wise.
#'
#' A parameter that no step responds to draws a warning and is otherwise
#' ignored. [summary()] on a design lists the names that are available.
#'
#' An atomic vector always supplies one value per design, so a parameter that
#' is itself a vector has to be wrapped: `prob_each = list(c(0, .5, .5))` is
#' one design, where `prob_each = c(0, .5, .5)` is three. Handing a bare
#' vector to a parameter that currently holds one warns.
#'
#' Only bare vectors and bare lists are read that way. A data frame, a matrix
#' and anything carrying a class are single replacement values, so a design
#' written as `declare_model(data = pilot, ...)` swaps its data with
#' `redesign(design, pilot = new_df)` and needs no wrapping. A redesign
#' reaches the name the design reads the data under, not `data`, which names
#' fabricate's argument and belongs to the declaration.
#'
#' @family modifying a design
#' @param .design A `design`. Named with a dot, like `.method` and `.summary`
#'   in [declare_estimator()], because everything else here is a parameter of
#'   the user's design: a plain `design` would partially match and swallow a
#'   parameter named `d`, `de`, `des`, `desi` or `desig`, and designs with a
#'   parameter named `d` exist.
#' @param ... Named parameter values. A bare atomic vector supplies one design
#'   per element; a data frame, a matrix, a function or any classed object is
#'   one value. To sweep over such values, pass a list of them.
#' @param .expand If `TRUE` (default), expand the parameter grid; if `FALSE`,
#'   zip parallel vectors. Dotted for the same reason as `.design`: an
#'   undotted `expand` after the dots would collide exactly with a parameter
#'   of that name.
#' @return A single `design` if one combination is supplied, otherwise a list
#'   of designs named `design_1`, `design_2`, etc.
#' @export
#' @examples
#' designer <- function(N) {
#'   declare_model(N = N, Y = rnorm(N)) +
#'     declare_inquiry(mu = mean(Y))
#' }
#' design <- designer(50)
#' redesigned <- redesign(design, N = c(10, 20))
#' length(redesigned)
#'
#' # a function-valued parameter
#' summarizer <- function(x) mean(x)
#' design <- declare_model(N = 50, Y = rnorm(N)) +
#'   declare_inquiry(mu = summarizer(Y))
#' redesign(design, summarizer = stats::median)
redesign <- function(.design, ..., .expand = TRUE) {
  design <- .design
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`.design` must be a `design` or `design_step` object.")
  }
  new_params <- list(...)
  if (length(new_params) == 0) return(design)
  # `find_all_objects()` is the definition of what a design's parameters are,
  # and every check here uses it rather than a rule of its own. `include_unbound`
  # adds the names a design expects `redesign()` to supply, which the printed
  # table leaves out because most of them are columns.
  reachable <- unique(find_all_objects(design, include_unbound = TRUE)$name)
  # The refusal first: a name this design writes down gets the message that
  # says how to make it a parameter, not a warning that it cannot be found
  # followed by that message.
  # Most specific refusal first. A note gets the message about notes, a name
  # the design writes down gets the message about declaring it, and only a
  # name that is neither gets the generic "not found" warning.
  check_params_are_not_notes(design, names(new_params))
  check_params_are_declared(design, names(new_params), reachable)
  check_params_in_design(design, names(new_params), reachable)
  check_param_vectors(design, new_params)
  param_df <- param_grid(new_params, expand = .expand)
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- extract_param_row(param_df, i)
    d <- modify_design_params(design, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  names(designs) <- paste0("design_", seq_along(designs))
  designs
}

#' Extract a single row of a parameter data frame as a clean named list
#'
#' Atomic columns are returned as scalar values; list-columns have their
#' singleton element unwrapped, so a row whose `fn` is a list of one
#' function appears as a function (not a 1-element list).
#'
#' @keywords internal
#' @noRd
extract_param_row <- function(param_df, i) {
  out <- list()
  for (nm in names(param_df)) {
    col <- param_df[[nm]]
    if (is.list(col)) out[[nm]] <- col[[i]] else out[[nm]] <- col[i]
  }
  out
}

#' Build a family of designs from a designer function
#'
#' @family modifying a design
#' @param .designer A function returning a `design`. Dotted for the same
#'   reason as [redesign()]'s `.design`.
#' @param ... Named parameter values to vary.
#' @param .expand If `TRUE`, expand the grid; if `FALSE`, zip values.
#' @return A single design or a list of designs.
#' @export
#' @examples
#' designer <- function(N) declare_model(N = N, Y = rnorm(N))
#' expand_design(designer, N = c(10, 20))
expand_design <- function(.designer, ..., .expand = TRUE) {
  new_params <- list(...)
  param_df <- param_grid(new_params, expand = .expand)
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- extract_param_row(param_df, i)
    d <- do.call(.designer, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  setNames(designs, paste0("design_", seq_along(designs)))
}
