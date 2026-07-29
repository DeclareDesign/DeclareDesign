#' Rebuild a step's closure after parameter rebinding
#'
#' Reconstructs the execution function so the new dots take effect.
#'
#' @keywords internal
#' @noRd
rebuild_step <- function(step, new_dots) {
  step_type   <- attr(step, "step_type")
  causal_type <- attr(step, "causal_type")
  label       <- attr(step, "label")
  call        <- attr(step, "call")
  handler_expr <- attr(step, "handler_expr")
  new_fn <- switch(
    step_type,
    "model"       = make_fabricate_step(new_dots, id_label_na = FALSE),
    "measurement" = make_fabricate_step(new_dots, id_label_na = TRUE),
    "assignment"  = make_fabricate_step(new_dots, id_label_na = TRUE),
    "sampling"    = make_sampling_step(new_dots, attr(step, "filter_quo")),
    "inquiry"     = make_inquiry_step(new_dots, attr(step, "subset_quo"), label,
                                       handler = attr(step, "handler_fn")),
    "estimator"   = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = attr(step, "inquiry_arg"),
      term        = attr(step, "term_arg"),
      add_inquiry = TRUE,
      handler     = attr(step, "handler_fn")
    ),
    "test"        = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = NULL,
      term        = attr(step, "term_arg"),
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
      handler <- attr(step, "handler_fn")
      d <- new_dots
      function(data) {
        if (handler_is_fabricate(handler)) {
          rlang::inject(handler(data = data, !!!d))
        } else {
          args <- lapply(d, function(q) {
            rlang::eval_tidy(q, data = if (is.data.frame(data)) as.list(data) else NULL)
          })
          do.call(handler, c(list(data), args))
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
  carry <- c("filter_quo", "subset_quo", "method_arg", "summary_arg",
             "inquiry_arg", "term_arg", "handler_fn", "draws", "method_name")
  for (nm in carry) {
    if (!is.null(attr(step, nm))) attr(out, nm) <- attr(step, nm)
  }
  out
}

#' Rebind parameters in a single design
#'
#' @keywords internal
#' @noRd
modify_design_params <- function(design, params) {
  new_steps <- lapply(unclass(design), function(step) {
    dots <- attr(step, "dots")
    filter_quo <- attr(step, "filter_quo")
    subset_quo <- attr(step, "subset_quo")
    if ((is.null(dots) || length(dots) == 0) &&
        is.null(filter_quo) && is.null(subset_quo)) {
      return(step)
    }
    new_dots <- dots
    new_filter <- filter_quo
    new_subset <- subset_quo
    changed <- FALSE
    for (param_name in names(params)) {
      new_val <- params[[param_name]]
      if (length(new_dots) > 0) {
        for (j in seq_along(new_dots)) {
          q <- new_dots[[j]]
          # Case 1: the dot is named after the parameter (e.g. N = 100 -> N = 200)
          if (!is.null(names(new_dots)) && identical(names(new_dots)[j], param_name)) {
            new_dots[[j]] <- rlang::new_quosure(
              rlang::expr(!!new_val),
              env = rlang::quo_get_env(q)
            )
            changed <- TRUE
          } else {
            # Case 2: parameter appears as a free symbol in the quosure's expr
            # or environment chain. In either case we clone the env and bind
            # the new value so subsequent eval_tidy() resolves it.
            expr <- rlang::quo_get_expr(q)
            if (quo_uses_param(q, param_name)) {
              env <- rlang::quo_get_env(q)
              new_env <- rlang::env_clone(env)
              rlang::env_bind(new_env, !!param_name := new_val)
              new_dots[[j]] <- rlang::new_quosure(expr, env = new_env)
              changed <- TRUE
            }
          }
        }
      }
      if (!is.null(new_filter)) {
        env <- rlang::quo_get_env(new_filter)
        expr <- rlang::quo_get_expr(new_filter)
        if (quo_uses_param(new_filter, param_name)) {
          new_env <- rlang::env_clone(env)
          rlang::env_bind(new_env, !!param_name := new_val)
          new_filter <- rlang::new_quosure(expr, env = new_env)
          changed <- TRUE
        }
      }
      if (!is.null(new_subset)) {
        env <- rlang::quo_get_env(new_subset)
        expr <- rlang::quo_get_expr(new_subset)
        if (quo_uses_param(new_subset, param_name)) {
          new_env <- rlang::env_clone(env)
          rlang::env_bind(new_env, !!param_name := new_val)
          new_subset <- rlang::new_quosure(expr, env = new_env)
          changed <- TRUE
        }
      }
    }
    if (!changed) return(step)
    out_step <- rebuild_step(step, new_dots)
    if (!is.null(new_filter)) attr(out_step, "filter_quo") <- new_filter
    if (!is.null(new_subset)) attr(out_step, "subset_quo") <- new_subset
    if (identical(attr(step, "step_type"), "sampling")) {
      out_step <- structure(
        make_sampling_step(new_dots, new_filter),
        attributes = attributes(out_step)
      )
      out_step <- build_step(
        fn = make_sampling_step(new_dots, new_filter),
        handler_expr = attr(step, "handler_expr"),
        dots = new_dots,
        step_type = "sampling",
        causal_type = "dgp",
        label = attr(step, "label"),
        call = attr(step, "call"),
        filter_quo = new_filter
      )
    }
    if (identical(attr(step, "step_type"), "inquiry")) {
      out_step <- build_step(
        fn = make_inquiry_step(new_dots, new_subset, attr(step, "label"),
                                handler = attr(step, "handler_fn")),
        handler_expr = attr(step, "handler_expr"),
        dots = new_dots,
        step_type = "inquiry",
        causal_type = "inquiry",
        label = attr(step, "label"),
        call = attr(step, "call"),
        subset_quo = new_subset,
        handler_fn = attr(step, "handler_fn")
      )
    }
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
  dots <- attr(step, "dots")
  if (name %in% names(dots)) return(TRUE)
  quos <- c(as.list(dots), list(attr(step, "filter_quo"), attr(step, "subset_quo")))
  quos <- Filter(rlang::is_quosure, quos)
  any(vapply(quos, quo_uses_param, logical(1), name = name))
}

#' Warn about requested parameters no step would respond to
#'
#' @keywords internal
#' @noRd
check_params_in_design <- function(design, param_names) {
  steps <- unclass(design)
  found <- vapply(param_names, function(name) {
    any(vapply(steps, step_uses_param, logical(1), name = name))
  }, logical(1))
  missing <- param_names[!found]
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
#' `redesign(design, N = c(50, 100))` means two designs. That rule is
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

#' Test whether an environment chain contains a binding
#'
#' @keywords internal
#' @noRd
env_has_var <- function(env, name) {
  if (!rlang::is_environment(env)) return(FALSE)
  tryCatch(
    rlang::env_has(env, name, inherit = TRUE)[[1]],
    error = function(e) FALSE
  )
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
#' An atomic vector supplies one value per element. Anything else (a function,
#' a formula, an environment) is a single value, since `length()` does not
#' index it. To vary a non-atomic parameter, pass a list of values.
#'
#' @keywords internal
#' @noRd
as_param_list <- function(v) {
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
           "`expand = FALSE`.")
    }
    as.data.frame(lapply(lens, function(l) if (l == 1L) rep(1L, n) else seq_len(l)))
  }
  out <- purrr::imap(cols, function(col, nm) simplify_param_col(col[idx[[nm]]]))
  tibble::as_tibble(out)
}

#' Re-parameterize a design
#'
#' Replaces parameter values in the captured environments of a design's steps,
#' producing one or more modified designs. With `expand = TRUE` (the default),
#' the cross-product of parameter values is taken; with `expand = FALSE`,
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
#' @param design A `design`.
#' @param ... Named parameter values. An atomic vector supplies one design per
#'   element; to vary a function or another non-atomic parameter, pass a list
#'   of values.
#' @param expand If `TRUE` (default), expand the parameter grid; if `FALSE`,
#'   zip parallel vectors.
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
redesign <- function(design, ..., expand = TRUE) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  new_params <- list(...)
  if (length(new_params) == 0) return(design)
  check_params_in_design(design, names(new_params))
  check_param_vectors(design, new_params)
  param_df <- param_grid(new_params, expand = expand)
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
#' @param designer A function returning a `design`.
#' @param ... Named parameter values to vary.
#' @param expand If `TRUE`, expand the grid; if `FALSE`, zip values.
#' @return A single design or a list of designs.
#' @export
#' @examples
#' designer <- function(N) declare_model(N = N, Y = rnorm(N))
#' expand_design(designer, N = c(10, 20))
expand_design <- function(designer, ..., expand = TRUE) {
  new_params <- list(...)
  param_df <- param_grid(new_params, expand = expand)
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- extract_param_row(param_df, i)
    d <- do.call(designer, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  setNames(designs, paste0("design_", seq_along(designs)))
}
