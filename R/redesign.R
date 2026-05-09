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
    "inquiry"     = make_inquiry_step(new_dots, attr(step, "subset_quo"), label),
    "estimator"   = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = attr(step, "inquiry_arg"),
      term        = attr(step, "term_arg"),
      add_inquiry = TRUE
    ),
    "test"        = make_estimator_step(
      method      = attr(step, "method_arg"),
      summary_fn  = attr(step, "summary_arg"),
      dots        = new_dots,
      label       = label,
      inquiry     = NULL,
      term        = attr(step, "term_arg"),
      add_inquiry = FALSE
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
        args <- lapply(d, function(q) rlang::eval_tidy(q, data = data))
        do.call(handler, c(list(data), args))
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
             "inquiry_arg", "term_arg", "handler_fn")
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
            # Case 2: parameter appears as a free symbol in another quosure's env
            env <- rlang::quo_get_env(q)
            if (env_has_var(env, param_name)) {
              new_env <- rlang::env_clone(env)
              rlang::env_bind(new_env, !!param_name := new_val)
              new_dots[[j]] <- rlang::new_quosure(rlang::quo_get_expr(q),
                                                  env = new_env)
              changed <- TRUE
            }
          }
        }
      }
      if (!is.null(new_filter)) {
        env <- rlang::quo_get_env(new_filter)
        if (env_has_var(env, param_name)) {
          new_env <- rlang::env_clone(env)
          rlang::env_bind(new_env, !!param_name := new_val)
          new_filter <- rlang::new_quosure(rlang::quo_get_expr(new_filter),
                                            env = new_env)
          changed <- TRUE
        }
      }
      if (!is.null(new_subset)) {
        env <- rlang::quo_get_env(new_subset)
        if (env_has_var(env, param_name)) {
          new_env <- rlang::env_clone(env)
          rlang::env_bind(new_env, !!param_name := new_val)
          new_subset <- rlang::new_quosure(rlang::quo_get_expr(new_subset),
                                            env = new_env)
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
        fn = make_inquiry_step(new_dots, new_subset, attr(step, "label")),
        handler_expr = attr(step, "handler_expr"),
        dots = new_dots,
        step_type = "inquiry",
        causal_type = "inquiry",
        label = attr(step, "label"),
        call = attr(step, "call"),
        subset_quo = new_subset
      )
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
  new_params <- list(...)
  if (length(new_params) == 0) return(design)
  param_df <- if (expand) {
    expand.grid(new_params, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  } else {
    as.data.frame(new_params, stringsAsFactors = FALSE)
  }
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- as.list(param_df[i, , drop = FALSE])
    d <- modify_design_params(design, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  names(designs) <- paste0("design_", seq_along(designs))
  designs
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
    as.data.frame(new_params, stringsAsFactors = FALSE)
  }
  designs <- purrr::map(seq_len(nrow(param_df)), function(i) {
    params_i <- as.list(param_df[i, , drop = FALSE])
    d <- do.call(designer, params_i)
    attr(d, "parameters") <- param_df[i, , drop = FALSE]
    d
  })
  if (length(designs) == 1L) return(designs[[1]])
  setNames(designs, paste0("design_", seq_along(designs)))
}
