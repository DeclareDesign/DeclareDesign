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
#' is no handler to defer to: DeclareDesignZero *is* the thing doing the
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
#' @keywords internal
#' @noRd
dots_env <- function(dots, default = rlang::caller_env()) {
  if (length(dots) == 0) return(default)
  rlang::quo_get_env(dots[[1L]])
}

#' Get the simulations table from a diagnosis
#'
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

