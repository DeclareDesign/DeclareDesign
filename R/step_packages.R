#' The attached package a name resolves to, if any
#'
#' Walks from `env` towards the empty environment and reports the package whose
#' search-path entry (`package:stringr`) supplies `name`. Returns `NULL` for a
#' name the user binds, for base, and for anything a namespace resolves: a
#' design declared inside a package's function reaches that package's imports
#' through its namespace, which loads by itself, so nothing needs attaching.
#'
#' @keywords internal
#' @noRd
attached_package_of <- function(env, name) {
  while (rlang::is_environment(env) && !identical(env, rlang::empty_env())) {
    if (isNamespace(env) || identical(env, baseenv())) return(NULL)
    if (rlang::env_has(env, name)) {
      env_name <- environmentName(env)
      if (!startsWith(env_name, "package:")) return(NULL)
      pkg <- sub("^package:", "", env_name)
      return(if (identical(pkg, "base")) NULL else pkg)
    }
    env <- rlang::env_parent(env)
  }
  NULL
}

#' The names a step reads from attached packages
#'
#' Recorded when the step is declared, because that is the only moment the
#' answer is known: a design read back with `readRDS()` in a session that has
#' not attached stringr has no way to say where `str_to_upper` came from. The
#' walk follows user-written helpers into their bodies, as capture does, so a
#' package call made inside a helper is recorded against the step that uses it.
#'
#' Only names are recorded, never the functions. Nothing is loaded on the
#' strength of this record; [warn_unattached_packages()] reads it to say what
#' to attach.
#'
#' @param step A `design_step`.
#' @return A named character vector, names to packages, possibly empty.
#' @keywords internal
#' @noRd
step_packages <- function(step) {
  found <- character(0)
  seen <- new.env(parent = emptyenv())
  pending <- c(
    lapply(step_quosures(step), function(q) {
      list(names = expr_symbols(rlang::quo_get_expr(q)),
           env = rlang::quo_get_env(q))
    }),
    lapply(step_user_functions(step), function(fn) {
      list(names = closure_symbols(fn), env = environment(fn))
    })
  )
  while (length(pending)) {
    item <- pending[[1L]]
    pending <- pending[-1L]
    for (name in unique(item$names)) {
      if (!nzchar(name) || name %in% names(found)) next
      pkg <- attached_package_of(item$env, name)
      if (!is.null(pkg)) {
        found[[name]] <- pkg
        next
      }
      if (rlang::env_has(seen, name)) next
      assign(name, TRUE, envir = seen)
      binding <- user_binding_env(item$env, name)
      if (is.null(binding) || !rlang::env_has(binding, name)) next
      value <- tryCatch(rlang::env_get(binding, name), error = function(e) NULL)
      if (!is.function(value) || fn_is_from_package(value)) next
      pending <- c(pending, list(list(names = closure_symbols(value),
                                      env = environment(value))))
    }
  }
  found
}

#' Warn about recorded package names the session cannot find
#'
#' Called once at each entry point ([draw_data()], [run_design()],
#' [simulate_design()] and the rest), never inside the simulation loop, and
#' only over the steps that call will run, so `draw_data()` says nothing about
#' a package only the estimator needs. A design saved before steps recorded
#' their packages has no record and passes silently.
#'
#' The test is whether the name can be found from `globalenv()`, not whether
#' the package is attached: a record is only ever made for a name that
#' resolved through the search path, and `stringr::str_to_upper()` written out
#' in full is never recorded at all.
#'
#' @param designs A `design`, a `design_step`, or a list of designs.
#' @param what The causal types of the steps that will run.
#' @keywords internal
#' @noRd
warn_unattached_packages <- function(designs,
                                     what = c("dgp", "inquiry", "estimator")) {
  if (inherits(designs, c("design", "design_step"))) designs <- list(designs)
  steps <- unlist(lapply(designs, function(d) {
    if (inherits(d, "design_step")) list(d) else unclass(d)
  }), recursive = FALSE)
  steps <- Filter(function(step) {
    ct <- attr(step, "causal_type")
    is.null(ct) || ct %in% what
  }, steps)
  missing <- unresolved_packages(steps)
  if (!length(missing)) return(invisible(NULL))
  by_pkg <- split(names(missing), missing)
  bullets <- vapply(names(by_pkg), function(pkg) {
    paste0(pkg, ": ", paste0("`", by_pkg[[pkg]], "`", collapse = ", "))
  }, character(1))
  names(bullets) <- rep("*", length(bullets))
  rlang::warn(
    c("This design reads names from packages that are not attached.",
      bullets,
      i = attach_hint(missing)),
    class = "dd_unattached_packages"
  )
  invisible(NULL)
}

#' Recorded names the session cannot find, names to packages
#'
#' @param steps A list of `design_step`s.
#' @keywords internal
#' @noRd
unresolved_packages <- function(steps) {
  recorded <- unlist(unname(lapply(steps, function(step) attr(step, "packages"))))
  if (!length(recorded)) return(character(0))
  recorded <- recorded[!duplicated(names(recorded))]
  recorded[!vapply(names(recorded), exists, logical(1), envir = globalenv())]
}

#' `Attach them first: library(a); library(b).`
#' @keywords internal
#' @noRd
attach_hint <- function(missing) {
  paste0("Attach them first: ",
         paste0("library(", unique(missing), ")", collapse = "; "), ".")
}
