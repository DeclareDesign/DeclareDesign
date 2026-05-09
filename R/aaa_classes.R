#' @keywords internal
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
