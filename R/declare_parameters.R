#' Declare the parameters of a design
#'
#' @description
#' Names the quantities a design is built from, so that [redesign()] changes
#' them and nothing else. A parameter declared here is bound for every step
#' that follows, and takes precedence over a binding of the same name in the
#' workspace.
#'
#' Parameters are ordinary R expressions evaluated once, when the design is
#' built, and each may read the ones declared before it:
#' `declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1])`. They are not
#' redrawn on each simulation, which is what separates a parameter from a
#' random variable: `declare_parameters(u = rnorm(1))` fixes one number for
#' the life of the design.
#'
#' @details
#' Without a `declare_parameters()` step, a design's parameters are whatever
#' its expressions read out of the environment they were written in, and
#' `redesign()` finds them by name. That works and it stays supported:
#' `declare_model(N = n_units, ...)` with `n_units` defined outside the design
#' needs no declaration.
#'
#' What does need a name is a number written into the design. `declare_model(N
#' = 500)` puts 500 in the design, and nothing outside it names that number, so
#' `redesign(design, N = 1000)` errors rather than rewriting the argument
#' because its name matched. Giving the value a name outside the design fixes
#' that on its own, and is the usual answer; declaring it here earns its place
#' when several steps read the value, or when a column shares its name. Matching names is what used to make
#' `redesign(design, sd = 3)` write 3 into a column called `sd` instead of
#' changing the parameter behind it. Declaring separates the two: a redesign
#' changes the declared parameter, and a column of the same name is left
#' alone, because it belongs to the data rather than to the design.
#'
#' The step generates no data and is skipped when the design runs.
#'
#' @family design declarations
#' @param ... Named parameters. Each is evaluated once, in order, and may read
#'   the parameters declared before it.
#' @param label Step label.
#' @return A `design_step`.
#' @export
#' @examples
#' design <-
#'   declare_parameters(n_units = 100, effect = 0.5) +
#'   declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + effect) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = sample(rep(0:1, length.out = N))) +
#'   declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z)
#'
#' nrow(draw_data(design))
#' nrow(draw_data(redesign(design, n_units = 40)))
#'
#' # A later parameter may read an earlier one.
#' design <- declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1]) +
#'   declare_model(N = 60) +
#'   declare_assignment(Z = complete_ra(N, conditions = seq_len(m_arms)))
#' design_parameters(design)
declare_parameters <- function(..., label = "parameters") {
  dots <- rlang::enquos(...)
  nms <- names(dots) %||% rep("", length(dots))
  if (length(dots) && !all(nzchar(nms))) {
    stop("Every parameter must be named, as in `declare_parameters(N = 100)`.",
         call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    stop("Parameter declared more than once: ", paste(dup, collapse = ", "), ".",
         call. = FALSE)
  }
  dots <- capture_dots_env(dots)
  build_step(
    fn           = function(data) data,
    handler_expr = NULL,
    dots         = dots,
    step_type    = "parameters",
    causal_type  = "parameters",
    label        = label,
    call         = sys.call()
  )
}

#' Is this step a parameter declaration?
#'
#' @keywords internal
#' @noRd
is_parameters_step <- function(step) {
  identical(attr(step, "step_type"), "parameters")
}

#' Evaluate one parameter declaration into its values
#'
#' In order, each parameter evaluated with the ones before it in scope, so
#' `declare_parameters(m = 3, ks = seq_len(m))` resolves `ks`. The
#' already-evaluated parameters go in as a data mask rather than as bindings,
#' which leaves each quosure's own environment intact for everything else it
#' reads.
#'
#' @keywords internal
#' @noRd
parameter_values <- function(step) {
  dots <- attr(step, "dots")
  # Evaluated once and kept. A parameter is fixed for the life of the design,
  # so a stochastic one (`locality_shock = rnorm(500, sd = sqrt(ICC))`) must
  # not be redrawn every time the design is rebuilt, which `+` and every
  # redesign both do. Without the cache, redesigning `cluster_prob` silently
  # drew a new population.
  cached <- attr(step, "param_values")
  out <- if (is.list(cached)) cached[intersect(names(dots), names(cached))] else list()
  for (nm in names(dots)) {
    if (nm %in% names(out)) next
    out[nm] <- list(rlang::eval_tidy(dots[[nm]], data = out))
  }
  out[names(dots)]
}

#' Keep the values a parameter declaration evaluated to
#'
#' Called once, from [apply_parameters()], so every later rebuild reuses them.
#' [modify_design_params()] drops the ones a redesign invalidates.
#'
#' @keywords internal
#' @noRd
cache_parameter_values <- function(step, values) {
  attr(step, "param_values") <- values
  step
}

#' Drop the cached values a redesign invalidates
#'
#' A parameter may read the ones declared before it, so changing the `k`th
#' invalidates the `k`th onward and nothing before it. Redesigning `ICC` does
#' redraw the two shocks computed from it, which is the point; redesigning
#' `cluster_prob`, declared after them, leaves them alone.
#'
#' A value that is not actually different invalidates nothing. Macartan's rule:
#' redesigning a parameter to the value it already holds must not change what
#' the design does, and it would have, because the shocks downstream of it are
#' random.
#'
#' @param new_values Named list of the values the redesign supplied.
#' @keywords internal
#' @noRd
invalidate_param_cache <- function(step, new_values) {
  cached <- attr(step, "param_values")
  if (!is.list(cached)) return(step)
  nms <- names(attr(step, "dots"))
  moved <- names(new_values)[vapply(names(new_values), function(nm) {
    !nm %in% names(cached) || !identical(cached[[nm]], new_values[[nm]])
  }, logical(1))]
  if (!length(moved)) return(step)
  first <- min(match(moved, nms), na.rm = TRUE)
  if (!is.finite(first)) return(step)
  keep <- nms[seq_len(first - 1L)]
  attr(step, "param_values") <- cached[intersect(keep, names(cached))]
  step
}

#' Every parameter name declared anywhere in a design
#'
#' @keywords internal
#' @noRd
declared_param_names <- function(design) {
  steps <- if (inherits(design, "design")) unclass(design) else design
  nms <- lapply(steps, function(s) {
    if (!is_parameters_step(s)) return(character(0))
    names(attr(s, "dots")) %||% character(0)
  })
  unique(unlist(nms, use.names = FALSE)) %||% character(0)
}

#' Put the declared parameters in a user-written function's lookup chain
#'
#' A handler, a `.method` or a `.summary` is stored as a value, so a parameter
#' it reads out of its closure is beyond the reach of the expression rebinding
#' that reaches everything else. Giving the function a new environment whose
#' parent is its old one puts the parameters ahead of the workspace and leaves
#' every other name it reads exactly where it was.
#'
#' Only user-written functions are touched. Re-homing a package's function
#' would cut it off from its own namespace, so `fabricate()`, `lm_robust()`
#' and `pivot_wider()` are returned untouched, and so is a function whose body
#' reads none of the parameters.
#'
#' The environment this creates is marked, so a second parameter binding
#' replaces it rather than stacking another one on top of it.
#'
#' @keywords internal
#' @noRd
rehome_fn_on_params <- function(fn, params, marker = "dd_param_env") {
  if (!is.function(fn)) return(NULL)
  env <- environment(fn)
  if (!rlang::is_environment(env)) return(NULL)
  if (isTRUE(attr(env, marker))) env <- rlang::env_parent(env)
  if (is_package_env(env)) return(NULL)
  used <- intersect(names(params), expr_symbols(body(fn)))
  if (!length(used)) return(NULL)
  new_env <- rlang::new_environment(data = params, parent = env)
  attr(new_env, marker) <- TRUE
  environment(fn) <- new_env
  fn
}

#' Bind declared parameters into the expressions of one step
#'
#' The rebinding [redesign()] does, minus the branch that matches a parameter
#' against the *name* of a declared argument. That branch replaces the
#' argument's whole expression with the new value, which is what makes
#' `declare_model(N = 1, sd = sd^2)` collapse to `sd = 3` under
#' `redesign(sd = 3)`. A declared parameter needs none of it: it is reached as
#' a free symbol like any other name, and a column that shares its name keeps
#' its own expression.
#'
#' @keywords internal
#' @noRd
bind_params_into_step <- function(step, params, marker = "dd_param_env",
                                  applied_attr = "params_applied") {
  if (!length(params)) return(step)
  applied <- attr(step, applied_attr)
  if (!is.null(applied) && identical(applied, params)) return(step)
  dots <- attr(step, "dots")
  side <- lapply(stats::setNames(side_quo_names(), side_quo_names()),
                 function(nm) attr(step, nm))
  new_dots <- dots
  new_side <- side
  changed <- FALSE
  for (name in names(params)) {
    new_val <- params[[name]]
    for (j in seq_along(new_dots)) {
      rebound <- rebind_quo_param(new_dots[[j]], name, new_val)
      if (!is.null(rebound)) {
        new_dots[[j]] <- rebound
        changed <- TRUE
      }
    }
    for (nm in names(new_side)) {
      rebound <- rebind_quo_param(new_side[[nm]], name, new_val)
      if (!is.null(rebound)) {
        new_side[[nm]] <- rebound
        changed <- TRUE
      }
    }
  }
  # A handler is a value rather than an expression, so a parameter it reads
  # out of its closure is reached by re-homing the function, not by rebinding
  # a quosure. `rebuild_step()` reads these back off the step, so they are set
  # before it runs.
  for (nm in c("handler_fn", "method_arg", "summary_arg")) {
    rehomed <- rehome_fn_on_params(attr(step, nm), params, marker = marker)
    if (!is.null(rehomed)) {
      attr(step, nm) <- rehomed
      changed <- TRUE
    }
  }
  if (!changed) return(step)
  # Binding clones the environment of whichever quosure reads the name, which
  # leaves a parameter read by a later dot invisible to the step's executor.
  shared <- reshare_step_quos(dots, side, new_dots, new_side)
  out <- rebuild_step(step, shared$dots, shared$side)
  if (!is.null(attr(step, "draws"))) attr(out, "draws") <- attr(step, "draws")
  attr(out, applied_attr) <- params
  out
}

#' Push each parameter declaration onto the steps that follow it
#'
#' Called from [construct_design()], so a design carries its parameters
#' already bound however it was assembled. A design with no parameter
#' declaration returns untouched and never reaches the rebinding path at all,
#' which is what keeps this invisible to every design written before it
#' existed.
#'
#' A second `declare_parameters()` applies to the steps after itself only, so
#' redeclaring a name mid-design changes it from that point on rather than
#' retroactively.
#'
#' @keywords internal
#' @noRd
apply_parameters <- function(steps) {
  is_param <- vapply(steps, is_parameters_step, logical(1))
  if (!any(is_param)) return(steps)
  for (i in which(is_param)) {
    params <- parameter_values(steps[[i]])
    steps[[i]] <- cache_parameter_values(steps[[i]], params)
    if (!length(params)) next
    for (j in seq_along(steps)) {
      if (j <= i || is_param[[j]]) next
      steps[[j]] <- bind_params_into_step(steps[[j]], params)
    }
  }
  steps
}

#' Report the parameters a design can be redesigned on
#'
#' @description
#' Lists the names [redesign()] can change: the parameters a
#' [declare_parameters()] step declares, and the objects the design's
#' expressions read out of the environments they were written in.
#'
#' Symbols that resolve to a package (`rnorm`, `complete_ra`) are not
#' parameters and are left out, and neither are names that resolve to nothing,
#' because those are columns an earlier step created.
#'
#' @family modifying a design
#' @param design A `design` or a `design_step`.
#' @return A data frame with one row per name per step: `name`, `value_str`,
#'   `kind` (`scalar`, `vector`, `list`, `data`, `function` or `other`),
#'   `declared` (whether a `declare_parameters()` step declares it), and
#'   `step`. Rows are in step order.
#' @export
#' @examples
#' design <- declare_parameters(n_units = 100) +
#'   declare_model(N = n_units, Y = rnorm(N))
#' design_parameters(design)
design_parameters <- function(design) {
  find_all_objects(design)
}
