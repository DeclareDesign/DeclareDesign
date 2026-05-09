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
        if (identical(handler, fabricatrZero::fabricate)) {
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
            if (env_has_var(rlang::quo_get_env(q), param_name) ||
                expr_has_symbol(expr, param_name)) {
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
        if (env_has_var(env, param_name) || expr_has_symbol(expr, param_name)) {
          new_env <- rlang::env_clone(env)
          rlang::env_bind(new_env, !!param_name := new_val)
          new_filter <- rlang::new_quosure(expr, env = new_env)
          changed <- TRUE
        }
      }
      if (!is.null(new_subset)) {
        env <- rlang::quo_get_env(new_subset)
        expr <- rlang::quo_get_expr(new_subset)
        if (env_has_var(env, param_name) || expr_has_symbol(expr, param_name)) {
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

#' Zip a named list of parameter vectors into a row-wise data frame
#'
#' Length-1 entries are recycled. Returns a data frame with one row per
#' position. List columns are preserved (so functions, list-valued params,
#' etc. survive).
#'
#' @keywords internal
#' @noRd
zip_params <- function(params) {
  if (length(params) == 0L) return(data.frame())
  lengths_v <- vapply(params, length, integer(1))
  n <- max(lengths_v)
  if (any(lengths_v != 1L & lengths_v != n)) {
    stop("All parameter vectors must have length 1 or the same length when ",
         "`expand = FALSE`.")
  }
  # Recycle scalars and store as list-cols for non-atomic cases.
  cols <- lapply(params, function(v) {
    if (length(v) == 1L) v <- rep(list(v[[1]]), n) else if (!is.list(v)) v <- as.list(v)
    v
  })
  # Try to simplify obvious atomic cases back to vectors so users see plain df
  cols <- lapply(cols, function(col) {
    if (all(vapply(col, function(x) is.atomic(x) && length(x) == 1L, logical(1)))) {
      unlist(col)
    } else {
      col
    }
  })
  out <- tibble::as_tibble(cols)
  out
}

#' Re-parameterize a design
#'
#' Replaces parameter values in the captured environments of a design's steps,
#' producing one or more modified designs. With `expand = TRUE` (the default),
#' the cross-product of parameter values is taken; with `expand = FALSE`,
#' values are zipped position-wise.
#'
#' @param design A `design`.
#' @param ... Named parameter values.
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
redesign <- function(design, ..., expand = TRUE) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  new_params <- list(...)
  if (length(new_params) == 0) return(design)
  param_df <- if (expand) {
    expand.grid(new_params, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  } else {
    zip_params(new_params)
  }
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
  param_df <- if (expand) {
    expand.grid(new_params, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  } else {
    zip_params(new_params)
  }
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- extract_param_row(param_df, i)
    d <- do.call(designer, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  setNames(designs, paste0("design_", seq_along(designs)))
}
