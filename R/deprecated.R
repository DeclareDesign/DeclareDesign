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
    paste0("`", fn_name, "()` is not implemented in DeclareDesignZero."),
    "i" = "Share the R script instead of the design object.",
    "i" = "To compare what two designs achieve, use `compare_diagnoses()`."
  ))
}

#' Defunct comparison helpers
#'
#' These functions existed in the original DeclareDesign but are not part of
#' DeclareDesignZero. Each compared two designs by printing their code, their
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

#' Deprecated: select diagnosands by name
#'
#' `select_diagnosands()` is retained so existing scripts keep running. Both of
#' its jobs have simpler spellings now. To build a set from the stock
#' definitions, pass their names to [diagnose_design()] directly. To trim a set
#' of your own, declare the one you want, or join what you want to the defaults
#' with `+`.
#'
#' ```
#' # old
#' diagnose_design(design, diagnosands = select_diagnosands("bias", "rmse"))
#' # new
#' diagnose_design(design, diagnosands = c("bias", "rmse"))
#' ```
#'
#' @param ... Diagnosand names, optionally preceded by a `diagnosands` object
#'   to subset.
#' @param alpha,subset,na.rm Passed through when building from stock names.
#' @return A `diagnosands` object.
#' @export
#' @examples
#' suppressWarnings(select_diagnosands("bias", "rmse"))
select_diagnosands <- function(..., alpha = 0.05, subset = NULL,
                               na.rm = FALSE) {
  rlang::warn(
    c("`select_diagnosands()` is deprecated.",
      "i" = 'Name them directly: `diagnose_design(design, diagnosands = c("bias", "rmse"))`.',
      "i" = "Or join your own to the defaults: `default_diagnosands() + declare_diagnosands(...)`."),
    .frequency = "once",
    .frequency_id = "select_diagnosands"
  )
  args <- list(...)
  base_set <- NULL
  if (length(args) > 0 && inherits(args[[1]], "diagnosands")) {
    base_set <- args[[1]]
    args <- args[-1]
  }
  keep <- unlist(args, use.names = FALSE)
  if (!is.character(keep) || length(keep) == 0) {
    rlang::abort("Name at least one diagnosand to keep, as a string.")
  }
  if (!is.null(base_set)) {
    unknown <- setdiff(keep, names(base_set))
    if (length(unknown) > 0) {
      rlang::abort(paste0(
        "Diagnosand", if (length(unknown) > 1) "s" else "", " not in this set: ",
        paste(unknown, collapse = ", "), "."
      ))
    }
    return(new_diagnosands(diagnosand_dots(base_set)[keep],
                           subset_quo = attr(base_set, "subset_quo")))
  }
  out <- stock_diagnosands(keep, alpha = alpha, na.rm = na.rm)
  subset_quo <- unwrap_quosure(rlang::enquo(subset))
  if (is.null(subset_quo)) return(out)
  new_diagnosands(diagnosand_dots(out), subset_quo = subset_quo)
}

#' Defunct: attach diagnosands to a design
#'
#' Diagnosands are not part of a design. Diagnosis is what you do to a design,
#' and two people can diagnose the same design differently without disagreeing
#' about the design itself, so the set belongs at the point of diagnosis.
#'
#' @param ... Ignored.
#' @return Never returns.
#' @export
set_diagnosands <- function(...) {
  rlang::abort(c(
    "`set_diagnosands()` is not implemented: diagnosands are not part of a design.",
    "i" = "Pass them where the diagnosis happens: `diagnose_design(design, diagnosands = ...)`."
  ))
}
