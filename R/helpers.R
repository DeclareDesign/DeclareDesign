#' Population variance
#'
#' Computes the (uncorrected) variance: the mean squared deviation from the
#' mean. Useful for design-based standard errors.
#'
#' @param x A numeric vector.
#' @return A scalar.
#' @export
#' @examples
#' pop.var(c(1, 2, 3, 4, 5))
pop.var <- function(x) mean((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)

# Pick the best available map function for the simulation loop.
# Only switches to furrr::future_map when (a) furrr is installed AND (b) the
# active future plan is actually parallel. Under the default sequential plan,
# furrr adds overhead with no benefit. With a parallel plan, furrr::future_map
# is used with seed = TRUE for statistically valid parallel RNG (L'Ecuyer-CMRG).
# Users enable parallelism with future::plan(multisession, workers = N) before
# calling simulate_design() -- no other changes needed.
sim_map_fn <- function(label = NULL) {
  base_map <- sim_base_map_fn()
  function(x, f, ...) {
    tick <- dd_progressor(length(x), label)
    base_map(x, function(...) {
      tick()
      f(...)
    }, ...)
  }
}

#' A progress ticker for one simulation loop
#'
#' Signals progress with progressr and renders none of it. progressr emits
#' conditions and leaves the display to whatever handler the *user* has
#' installed, so with no handler this writes nothing at all, in any context. A
#' handler is never installed here: a package that installs one surprises
#' people and fights whatever they set themselves.
#'
#' progressr is chosen over `purrr::map(.progress =)` for two reasons that a
#' console bar cannot cover. It relays out of `furrr` workers, so the parallel
#' path, where a run is long enough to want a progress bar in the first place,
#' is the same mechanism as the sequential one. And `progressr::handler_shiny()`
#' drives a Shiny app's own progress widget, where a cli bar would go to the
#' server console and never reach the browser.
#'
#' @param n Number of steps.
#' @param label Optional message shown beside the bar.
#' @return A function of no arguments; calling it advances one step.
#' @keywords internal
#' @noRd
dd_progressor <- function(n, label = NULL) {
  quiet <- function(...) invisible(NULL)
  if (!isTRUE(getOption("DeclareDesign.progress", TRUE))) return(quiet)
  if (!requireNamespace("progressr", quietly = TRUE)) return(quiet)
  if (!is.finite(n) || n < 1) return(quiet)
  # envir = parent.frame(2) so the progressor belongs to the caller of
  # sim_map_fn()'s returned closure, which is the frame that lives for the
  # whole loop; tying it to this frame would finish it immediately.
  tryCatch(
    progressr::progressor(steps = n, message = label, envir = parent.frame(2)),
    error = function(e) quiet
  )
}

#' Run `expr` with progress displayed, if progressr is available
#'
#' Backs the `progress` argument on [simulate_design()] and
#' [diagnose_design()], for the reader who wants a bar once without learning
#' that progressr exists. Global opt-in stays the better route:
#' `progressr::handlers(global = TRUE)` once per session.
#'
#' @keywords internal
#' @noRd
with_dd_progress <- function(expr) {
  if (!requireNamespace("progressr", quietly = TRUE)) {
    warning(
      "`progress = TRUE` needs the progressr package; continuing without it.",
      call. = FALSE
    )
    return(expr)
  }
  progressr::with_progress(expr)
}

#' The map function itself, before progress is layered on
#' @keywords internal
#' @noRd
sim_base_map_fn <- function() {
  has_furrr <- requireNamespace("furrr", quietly = TRUE)
  has_future <- requireNamespace("future", quietly = TRUE)
  if (has_furrr && has_future &&
      !inherits(future::plan(), "sequential")) {
    # Snapshot attached packages so workers can load the same ones.
    # This ensures functions like complete_ra(), lm_robust(), etc. referenced
    # in quosure environments are available on each worker.
    pkgs <- setdiff(
      sub("^package:", "", grep("^package:", search(), value = TRUE)),
      c("base", ".GlobalEnv", "Autoloads")
    )
    opts <- furrr::furrr_options(seed = TRUE, packages = pkgs)
    function(x, f, ...) furrr::future_map(x, f, ..., .options = opts)
  } else {
    purrr::map
  }
}

#' Capture variables as quosures
#'
#' Convenience alias for [rlang::quos()] used inside design declarations to
#' pass through bare expressions.
#'
#' @param ... Expressions.
#' @return A list of quosures.
#' @export
#' @examples
#' qs <- vars(x, y, z)
#' length(qs)
vars <- function(...) rlang::quos(...)

#' Take the dots as written, so the handler does its own evaluation
#'
#' Shared by [declare_estimator()] and [declare_step()], the two verbs that hand
#' their arguments to a function the user supplied. The rule those two follow is
#' that **whoever receives the argument evaluates it**: a bare `Y` arrives as the
#' name `Y`, which is what lets `lm_robust()` resolve `clusters` and `weights`
#' against the data, `rdss::rdrobust_helper()` do `pull(data, {{y}})`, and
#' `tidyr::pivot_wider()` select columns by name.
#'
#' The other verbs go the other way on purpose. `declare_inquiry()` and
#' `declare_diagnosands()` evaluate their expressions themselves, because there
#' is no handler to defer to: DeclareDesign *is* the thing doing the
#' computing, so `mean(Y_Z_1 - Y_Z_0)` has to become a number.
#'
#' Formulas are the one thing evaluated here, since `Y ~ Z` has to arrive as a
#' formula object carrying the environment it was written in.
#'
#' @param dots A named list of quosures.
#' @return A list of expressions ready to splice into a call.
#' @keywords internal
#' @noRd
dots_as_written <- function(dots) {
  lapply(dots, function(q) {
    e <- rlang::quo_get_expr(q)
    # `rlang::is_formula()` treats a `~` call as a formula before evaluation.
    if (rlang::is_formula(e)) {
      env <- rlang::quo_get_env(q)
      f <- eval(e, envir = env)
      if (is.null(environment(f))) environment(f) <- env
      return(f)
    }
    e
  })
}

#' The environment a step's arguments were written in
#'
#' `rlang::enquos()` captures every dot of one declaration with the same
#' environment, so the first one speaks for all of them. It is taken at
#' declaration time rather than at run time, so it is the environment the user
#' wrote the call in and not whatever is on the stack during a simulation.
#'
#' Two things keep the first dot speaking for the rest, and both are load
#' bearing: [capture_dots_env()] gives co-captured quosures one captured
#' environment, and [reshare_quo_envs()] puts them back on one after a
#' parameter has been bound into whichever of them reads it.
#'
#' @keywords internal
#' @noRd
dots_env <- function(dots, default = rlang::caller_env()) {
  if (length(dots) == 0) return(default)
  rlang::quo_get_env(dots[[1L]])
}

#' Get the simulations table from a diagnosis
#'
#' @family simulation and diagnosis
#' @param diagnosis A `diagnosis` object.
#' @return A tibble of simulations.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' get_simulations(d)
get_simulations <- function(diagnosis) diagnosis$simulations_df

#' Get the diagnosands table from a diagnosis
#'
#' @family simulation and diagnosis
#' @param diagnosis A `diagnosis` object.
#' @return A tibble of diagnosands.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' get_diagnosands(d)
get_diagnosands <- function(diagnosis) diagnosis$diagnosands_df


#' Warn once per run about estimator draws that failed
#'
#' One warning per run rather than one per draw: three failures out of five
#' hundred would otherwise be three warnings, and two hundred would be
#' unreadable. The first message is carried through, because a user whose
#' estimator failed two hundred times needs to know why and not only how often.
#'
#' @keywords internal
#' @noRd
warn_estimator_failures <- function(estimates_df, design_label = NULL) {
  if (!is.data.frame(estimates_df) || !nrow(estimates_df)) return(invisible(NULL))
  if (!"error" %in% names(estimates_df)) return(invisible(NULL))
  failed <- which(!is.na(estimates_df$error) & estimates_df$error)
  if (!length(failed)) return(invisible(NULL))
  labels <- estimates_df$estimator[failed]
  counts <- table(labels)
  who <- paste0(
    "`", names(counts), "` (", as.integer(counts), ")",
    collapse = ", "
  )
  first <- estimates_df$error_message[failed][[1]]
  rlang::warn(paste0(
    if (!is.null(design_label)) paste0(design_label, ": ") else "",
    length(failed), " estimator draw", if (length(failed) > 1) "s" else "",
    " failed and were recorded rather than run: ", who, ".\n",
    "First error: ", first, "\n",
    "Diagnosands are computed on the draws that succeeded, and `n_sims` ",
    "reports how many that was. Failed draws are not missing at random, so ",
    "please interpret with care."
  ))
}

#' Re-raise an estimator failure on a single run
#'
#' The step records a failure rather than aborting, which is what keeps a
#' 500-simulation diagnosis alive when one draw will not converge. A single
#' run is a different situation: somebody is debugging, and an NA row with a
#' message buried in a column is a worse answer than the error itself. So the
#' single-draw entry points re-raise, and only the simulation loop tolerates.
#'
#' @keywords internal
#' @noRd
stop_on_estimator_failure <- function(estimates_df) {
  if (!is.data.frame(estimates_df) || !nrow(estimates_df)) return(invisible(NULL))
  if (!"error" %in% names(estimates_df)) return(invisible(NULL))
  failed <- which(!is.na(estimates_df$error) & estimates_df$error)
  if (!length(failed)) return(invisible(NULL))
  rlang::abort(estimates_df$error_message[failed][[1]])
}

#' Evaluate a declared argument that may have been captured as a quosure
#'
#' Arguments a step holds as quosures (`term`, `inquiry`) are evaluated where
#' they were written, on every draw, so that a [redesign()] which rebinds a
#' name they read takes effect. A plain value passes through untouched, which
#' is what internal callers that build a step from an already-evaluated
#' argument supply.
#'
#' @keywords internal
#' @noRd
eval_step_arg <- function(x) {
  if (rlang::is_quosure(x)) rlang::eval_tidy(x) else x
}

#' The bindings one environment adds to another
#'
#' @keywords internal
#' @noRd
env_added_bindings <- function(new_env, old_env) {
  out <- list()
  if (identical(new_env, old_env)) return(out)
  for (nm in rlang::env_names(new_env)) {
    if (rlang::env_has(old_env, nm, inherit = FALSE) &&
        identical(rlang::env_get(old_env, nm), rlang::env_get(new_env, nm))) {
      next
    }
    out[nm] <- list(rlang::env_get(new_env, nm))
  }
  out
}

#' Put quosures that shared an environment back onto a shared one
#'
#' `capture_dots_env()` gives every quosure captured in one environment a
#' single captured environment, and `dots_env()` then reads a step's
#' environment off the first dot on the understanding that it speaks for all
#' of them. Binding a parameter clones the environment of the one quosure that
#' reads the name, which breaks that understanding:
#' `declare_estimator(Y ~ Z, se_type = se_type)` binds `se_type` into the
#' second dot's environment, the executor splices the arguments as written and
#' evaluates them in the *first* dot's environment, and the estimator dies
#' with `object 'se_type' not found` on every draw. Merging the added bindings
#' back onto one environment per original group restores it.
#'
#' Quosures that were captured separately keep their own environments, since
#' grouping is by the environment each one started in.
#'
#' @keywords internal
#' @noRd
reshare_quo_envs <- function(orig, rebound, marker = "dd_param_env") {
  idx <- which(vapply(orig, rlang::is_quosure, logical(1)) &
               vapply(rebound, rlang::is_quosure, logical(1)))
  if (length(idx) < 2L) return(rebound)
  handled <- rep(FALSE, length(idx))
  for (k in seq_along(idx)) {
    if (handled[[k]]) next
    base_env <- rlang::quo_get_env(orig[[idx[[k]]]])
    same <- vapply(idx, function(i) {
      identical(rlang::quo_get_env(orig[[i]]), base_env)
    }, logical(1))
    handled[same] <- TRUE
    group <- idx[same]
    if (length(group) < 2L) next
    added <- list()
    for (i in group) {
      new_binds <- env_added_bindings(rlang::quo_get_env(rebound[[i]]), base_env)
      for (nm in names(new_binds)) added[nm] <- list(new_binds[[nm]])
    }
    if (!length(added)) next
    merged <- rlang::env_clone(base_env)
    rlang::env_bind(merged, !!!added)
    attr(merged, marker) <- TRUE
    for (i in group) {
      rebound[[i]] <- rlang::new_quosure(rlang::quo_get_expr(rebound[[i]]),
                                         env = merged)
    }
  }
  rebound
}

#' Reshare environments across a step's dots and side quosures at once
#'
#' Returns the pair back in the shape the callers hold them, so a `NULL` set
#' of dots stays `NULL` rather than becoming an empty list.
#'
#' @keywords internal
#' @noRd
reshare_step_quos <- function(dots, side, new_dots, new_side) {
  n_dots <- length(new_dots)
  # `as.list()` because a dots object is a quosure list, which rlang refuses to
  # concatenate with the side quosures, some of which are NULL.
  merged <- reshare_quo_envs(
    c(as.list(dots), as.list(side)),
    c(as.list(new_dots), as.list(new_side))
  )
  # Assigned back element by element so the dots keep their class, and so a
  # side quosure that is absent stays absent rather than being dropped by a
  # `NULL` assignment.
  out_dots <- new_dots
  out_side <- new_side
  for (i in seq_len(n_dots)) {
    if (rlang::is_quosure(merged[[i]])) out_dots[[i]] <- merged[[i]]
  }
  for (i in seq_along(new_side)) {
    if (rlang::is_quosure(merged[[n_dots + i]])) {
      out_side[[i]] <- merged[[n_dots + i]]
    }
  }
  list(dots = out_dots, side = out_side)
}
