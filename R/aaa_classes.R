#' @details
#' A design is a sequence of steps combined with `+`. Each step is one of the
#' four elements of a research design:
#'
#' * [declare_model()] describes the world the design runs in.
#' * [declare_inquiry()] records the question. Its value on a draw is the estimand.
#' * [declare_sampling()], [declare_assignment()] and [declare_measurement()]
#'   make up the data strategy: who is observed, what they are assigned, and
#'   what is measured.
#' * [declare_estimator()] and [declare_test()] are the answer strategy.
#'
#' [diagnose_design()] simulates a design many times and reports how its
#' estimates compare to its estimands: bias, power, coverage and the rest.
#' [redesign()] changes a parameter and returns a new design, so two versions
#' can be compared before any data are collected.
#'
#' ```
#' design <-
#'   declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = sample(rep(0:1, length.out = N))) +
#'   declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")
#'
#' diagnose_design(design)
#' ```
"_PACKAGE"

#' Null-coalescing operator
#'
#' Returns `a` if it is non-NULL, otherwise `b`.
#'
#' @param a A value.
#' @param b A fallback value used when `a` is NULL.
#' @return Either `a` or `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build a design step
#'
#' Internal constructor for `design_step` objects. A design step is a function
#' of `data` carrying metadata used by the run loop, redesign machinery, and
#' diagnostic tooling.
#'
#' @param fn The execution closure of signature `function(data)`.
#' @param handler_expr Quoted expression for the handler used to rebuild the
#'   step under [redesign()].
#' @param dots Named list of quosures captured from the user.
#' @param step_type One of `"model"`, `"inquiry"`, `"assignment"`, `"sampling"`,
#'   `"measurement"`, `"estimator"`, `"test"`, `"diagnosand"`, `"custom"`.
#' @param causal_type One of `"dgp"`, `"inquiry"`, `"estimator"`, `"diagnosands"`.
#' @param label Step label.
#' @param call Originating call.
#' @param ... Additional attributes to attach to the step.
#' @return A `design_step` object.
#' @keywords internal
#' @noRd
build_step <- function(fn, handler_expr, dots, step_type, causal_type, label,
                       call, ...) {
  extra <- list(...)
  attrs <- list(
    class        = c("design_step", "dd", "function"),
    step_type    = step_type,
    causal_type  = causal_type,
    label        = label,
    call         = call,
    handler_expr = handler_expr,
    dots         = dots
  )
  attrs <- c(attrs, extra)
  for (nm in names(attrs)) {
    attr(fn, nm) <- attrs[[nm]]
  }
  fn
}

#' Build a design
#'
#' Internal constructor for `design` objects.
#'
#' @param steps A named list of `design_step` objects.
#' @return A `design`.
#' @keywords internal
#' @noRd
construct_design <- function(steps) {
  steps <- autolabel_estimators(steps)
  check_notes_against_params(steps)
  steps <- apply_parameters(steps)
  if (is.null(names(steps))) {
    names(steps) <- vapply(
      steps,
      function(s) attr(s, "label") %||% "step",
      character(1)
    )
  }
  names(steps) <- make.unique(names(steps), sep = "_")
  structure(steps, class = c("design", "dd"))
}

#' Infer a meaningful label for an estimator/test step
#'
#' Tier 1 uses the formula in the first quosure. Tier 2 appends the method
#' name (e.g., `"Y~Z (lm_robust)"`) when needed. Returning the existing
#' user-set label intact when no formula was supplied.
#'
#' @keywords internal
#' @noRd
infer_estimator_label <- function(step, include_method = FALSE) {
  dots <- attr(step, "dots")
  base <- attr(step, "label")
  formula_label <- NULL
  if (length(dots) > 0) {
    expr <- rlang::quo_get_expr(dots[[1]])
    if (rlang::is_formula(expr)) {
      formula_label <- gsub("\\s+", "", deparse(expr))
    }
  }
  if (is.null(formula_label)) return(base)
  method_name <- attr(step, "method_name")
  if (include_method && !is.null(method_name)) {
    paste0(formula_label, " (", method_name, ")")
  } else {
    formula_label
  }
}

#' Rebuild an estimator/test step with a new label
#'
#' Reconstructs the executor closure (which captures the label by value) so
#' the `estimator` column in simulation output reflects the new label rather
#' than the originally declared one.
#'
#' @keywords internal
#' @noRd
relabel_estimator_step <- function(step, new_label) {
  step_type <- attr(step, "step_type")
  add_inquiry <- identical(step_type, "estimator")
  inquiry_quo <- if (add_inquiry) attr(step, "inquiry_quo") else NULL
  fn <- make_estimator_step(
    method      = attr(step, "method_arg"),
    summary_fn  = attr(step, "summary_arg"),
    dots        = attr(step, "dots"),
    label       = new_label,
    inquiry     = inquiry_quo,
    term        = attr(step, "term_quo"),
    add_inquiry = add_inquiry,
    handler     = attr(step, "handler_fn")
  )
  out <- build_step(
    fn           = fn,
    handler_expr = attr(step, "handler_expr"),
    dots         = attr(step, "dots"),
    step_type    = step_type,
    causal_type  = attr(step, "causal_type"),
    label        = new_label,
    call         = attr(step, "call"),
    method_arg   = attr(step, "method_arg"),
    summary_arg  = attr(step, "summary_arg"),
    inquiry_quo  = inquiry_quo,
    term_quo     = attr(step, "term_quo"),
    handler_fn   = attr(step, "handler_fn"),
    method_name  = attr(step, "method_name")
  )
  if (!is.null(attr(step, "draws"))) {
    attr(out, "draws") <- attr(step, "draws")
  }
  out
}

#' Detect duplicate estimator labels and rename them
#'
#' Three-tier inference: (1) the formula expression, (2) formula plus method
#' name in parens when formulas alone collide, (3) `.a`, `.b`, `.c` suffixes
#' when truly identical estimators remain.
#'
#' @keywords internal
#' @noRd
autolabel_estimators <- function(steps) {
  is_est <- vapply(steps, function(s) {
    isTRUE(attr(s, "step_type") %in% c("estimator", "test"))
  }, logical(1))
  est_idx <- which(is_est)
  if (length(est_idx) < 2L) return(steps)

  original <- vapply(steps[est_idx], function(s) attr(s, "label"), character(1))
  if (length(unique(original)) == length(original)) return(steps)

  # Tier 1: formula-based label only when the original labels duplicate
  inferred <- vapply(steps[est_idx], infer_estimator_label, character(1),
                     include_method = FALSE)

  # Tier 2: append method name where formulas still collide
  if (any(duplicated(inferred))) {
    for (lbl in unique(inferred[duplicated(inferred)])) {
      idx <- which(inferred == lbl)
      with_method <- vapply(steps[est_idx[idx]], infer_estimator_label,
                            character(1), include_method = TRUE)
      if (length(unique(with_method)) > 1L) {
        inferred[idx] <- with_method
      }
    }
  }

  # Tier 3: append .a, .b, .c for any still-duplicate labels.
  # When the user supplied an explicit non-default label, prefer it as the base
  # so the suffixed labels look like `ols.a`, `ols.b` rather than `Y~Z.a`.
  default_labels <- c("estimator", "test")
  for (lbl in unique(inferred[duplicated(inferred)])) {
    idx <- which(inferred == lbl)
    base_for_suffix <- if (all(!original[idx] %in% default_labels) &&
                           length(unique(original[idx])) == 1L) {
      original[idx][1]
    } else {
      lbl
    }
    inferred[idx] <- paste0(base_for_suffix, ".", letters[seq_along(idx)])
  }

  changed <- inferred != original
  if (any(changed)) {
    pairs <- paste(
      sprintf("'%s' -> '%s'", original[changed], inferred[changed]),
      collapse = ", "
    )
    rlang::inform(paste0(
      "Estimator steps auto-labeled to ensure unique labels: ", pairs, ".\n",
      "Set `label =` explicitly to suppress this."
    ))
  }

  for (k in seq_along(est_idx)) {
    if (changed[k]) {
      i <- est_idx[k]
      steps[[i]] <- relabel_estimator_step(steps[[i]], inferred[k])
      if (!is.null(names(steps))) names(steps)[i] <- inferred[k]
    }
  }
  steps
}

#' Wrap a step in a named singleton list
#'
#' @param step A `design_step`.
#' @return A length-one named list whose name is the step's label.
#' @keywords internal
#' @noRd
wrap_step <- function(step) {
  nm <- attr(step, "label") %||% "step"
  setNames(list(step), nm)
}

#' Combine design steps into a design
#'
#' @description
#' The `+` operator concatenates `design_step` and `design` objects into a
#' single `design`. `design + NULL` is a no-op that returns the design
#' unchanged, which makes conditional step addition convenient.
#'
#' @param e1 A `design_step` or `design`.
#' @param e2 A `design_step`, `design`, or `NULL`.
#' @return A `design`.
#' @export
#' @method + dd
#' @examples
#' d <- declare_model(N = 50, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y))
#' length(d)
`+.dd` <- function(e1, e2) {
  if (is.null(e2)) {
    if (inherits(e1, "design")) return(e1)
    return(construct_design(wrap_step(e1)))
  }
  if (is.null(e1)) {
    if (inherits(e2, "design")) return(e2)
    return(construct_design(wrap_step(e2)))
  }
  steps1 <- if (inherits(e1, "design")) unclass(e1) else wrap_step(e1)
  steps2 <- if (inherits(e2, "design")) unclass(e2) else wrap_step(e2)
  steps <- c(steps1, steps2)
  construct_design(steps)
}
