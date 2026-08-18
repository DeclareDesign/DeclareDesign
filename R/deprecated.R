#' Deprecated alias for declare_model
#'
#' `declare_population()` is retained for compatibility with older scripts but
#' will be removed in a future version. Use [declare_model()] instead.
#'
#' @param ... Forwarded to [declare_model()].
#' @return A `design_step`.
#' @export
declare_population <- function(...) {
  rlang::warn(
    "`declare_population()` is deprecated. Use `declare_model()` instead.",
    .frequency = "once",
    .frequency_id = "declare_population"
  )
  declare_model(...)
}

#' Internal helper: emit a defunct error for the compare_* family
#'
#' @keywords internal
#' @noRd
.compare_defunct <- function(fn_name) {
  rlang::abort(c(
    paste0("`", fn_name, "()` is not implemented in DeclareDesign 2.0."),
    "i" = "Share the R script instead of the design object.",
    "i" = "To compare what two designs achieve, use `compare_diagnoses()`."
  ))
}

#' Defunct comparison helpers
#'
#' These functions existed in DeclareDesign 1.x but are not part of 2.0. Each
#' compared two designs by printing their code, their
#' draws, or their summaries side by side, which is a job for a diff of the
#' two scripts. They error with a message saying so.
#'
#' [compare_diagnoses()], which compares what two designs achieve rather than
#' how they are written, is implemented.
#'
#' @param ... Ignored.
#' @return Never returns.
#' @name compare_designs
#' @export
compare_designs          <- function(...) .compare_defunct("compare_designs")
#' @rdname compare_designs
#' @export
compare_design_code      <- function(...) .compare_defunct("compare_design_code")
#' @rdname compare_designs
#' @export
compare_design_data      <- function(...) .compare_defunct("compare_design_data")
#' @rdname compare_designs
#' @export
compare_design_estimates <- function(...) .compare_defunct("compare_design_estimates")
#' @rdname compare_designs
#' @export
compare_design_inquiries <- function(...) .compare_defunct("compare_design_inquiries")
#' @rdname compare_designs
#' @export
compare_design_summaries <- function(...) .compare_defunct("compare_design_summaries")
#' @rdname compare_designs
#' @export
print_code               <- function(...) .compare_defunct("print_code")

#' Deprecated alias for label_estimator
#'
#' `model_handler()` and `tidy_estimator()` are retained for compatibility but
#' will be removed in a future version. Use [label_estimator()] instead.
#'
#' @param ... Forwarded to [label_estimator()].
#' @return A function.
#' @export
model_handler <- function(...) {
  rlang::warn("`model_handler()` is deprecated. Use `label_estimator()` instead.")
  label_estimator(...)
}

#' @rdname model_handler
#' @export
tidy_estimator <- function(...) {
  rlang::warn("`tidy_estimator()` is deprecated. Use `label_estimator()` instead.")
  label_estimator(...)
}

#' Defunct step declarations from DeclareDesign 1.x
#'
#' `declare_potential_outcomes()` and `declare_reveal()` were separate steps in
#' 1.x. In 2.0 potential outcomes belong in [declare_model()] and revealing
#' belongs in [declare_measurement()], so neither has a 2.0 equivalent as a
#' step of its own.
#'
#' They exist here only so that 1.x code fails with a message saying what to
#' write instead. Without them R reports `could not find function`, which says
#' nothing about the replacement.
#'
#' @param ... Ignored.
#' @return Never returns; always raises an error.
#' @name declare-defunct
NULL

#' @rdname declare-defunct
#' @export
declare_potential_outcomes <- function(...) {
  rlang::abort(c(
    "`declare_potential_outcomes()` is not a step in DeclareDesign 2.0.",
    "i" = "Put potential outcomes in the model, as a formula:",
    "*" = "declare_model(N = 100, potential_outcomes(Y ~ 0.2 * Z + U))",
    "i" = "Named potential outcomes are ordinary model variables:",
    "*" = "declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.2)"
  ))
}

#' @rdname declare-defunct
#' @export
declare_reveal <- function(...) {
  rlang::abort(c(
    "`declare_reveal()` is not a step in DeclareDesign 2.0.",
    "i" = "Reveal outcomes in the measurement step:",
    "*" = "declare_measurement(Y = reveal_outcomes(Y ~ Z))",
    "i" = "Several assignment variables go on the right-hand side:",
    "*" = "declare_measurement(Y = reveal_outcomes(Y ~ A + B))"
  ))
}
