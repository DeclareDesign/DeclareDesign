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
  rlang::abort(paste0(
    "`", fn_name, "()` is not implemented in DeclareDesignZero. ",
    "Share the R script instead of the design object."
  ))
}

#' Defunct comparison helpers
#'
#' These functions existed in the original DeclareDesign but are not part of
#' DeclareDesignZero. They error with a message asking the user to share the
#' R script that defines the design.
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
compare_diagnoses        <- function(...) .compare_defunct("compare_diagnoses")
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
