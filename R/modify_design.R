#' Resolve a step locator to an integer index
#'
#' @keywords internal
#' @noRd
resolve_step_index <- function(design, step) {
  if (is.numeric(step)) return(as.integer(step))
  if (is.character(step)) {
    idx <- match(step, names(design))
    if (is.na(idx)) stop(sprintf("No step named %s.", step))
    return(idx)
  }
  if (inherits(step, "design_step")) {
    label <- attr(step, "label")
    idx <- match(label, names(design))
    if (is.na(idx)) stop(sprintf("Step labeled %s not found.", label))
    return(idx)
  }
  stop("`step` must be a label, an integer, or a design_step.")
}

#' Insert a step into a design
#'
#' @family modifying a design
#' @param design A `design`.
#' @param new_step A `design_step` to insert.
#' @param after,before A label, integer, or `design_step` indicating the
#'   anchor point. Provide exactly one of the two.
#' @return A `design`.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N)) +
#'   declare_inquiry(mu = mean(Y))
#' new_design <- insert_step(design,
#'   declare_measurement(Y2 = Y * 2),
#'   after = "model")
#' names(new_design)
insert_step <- function(design, new_step, after = NULL, before = NULL) {
  rlang::warn(
    "`insert_step()` is deprecated. Reconstruct the design explicitly instead.",
    .frequency = "once", .frequency_id = "insert_step"
  )
  if (is.null(after) && is.null(before)) {
    stop("Provide either `after` or `before`.")
  }
  steps <- unclass(design)
  anchor <- if (!is.null(after)) resolve_step_index(design, after) else
    resolve_step_index(design, before) - 1L
  if (anchor < 0) anchor <- 0L
  new_label <- attr(new_step, "label") %||% "step"
  before_part <- if (anchor > 0) steps[seq_len(anchor)] else list()
  after_part <- if (anchor < length(steps)) steps[seq.int(anchor + 1L, length(steps))] else list()
  insert_named <- setNames(list(new_step), new_label)
  combined <- c(before_part, insert_named, after_part)
  construct_design(combined)
}

#' Delete a step from a design
#'
#' @family modifying a design
#' @param design A `design`.
#' @param step A label, integer, or `design_step`.
#' @return A `design`.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N)) +
#'   declare_inquiry(mu = mean(Y))
#' delete_step(design, "mu") |> names()
delete_step <- function(design, step) {
  rlang::warn(
    "`delete_step()` is deprecated. Reconstruct the design explicitly instead.",
    .frequency = "once", .frequency_id = "delete_step"
  )
  steps <- unclass(design)
  idx <- resolve_step_index(design, step)
  steps <- steps[-idx]
  construct_design(steps)
}

#' Replace a step in a design
#'
#' @family modifying a design
#' @param design A `design`.
#' @param step A label, integer, or `design_step` to replace.
#' @param new_step The replacement `design_step`.
#' @return A `design`.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N)) +
#'   declare_inquiry(mu = mean(Y))
#' replace_step(design, "mu", declare_inquiry(med = median(Y)))
replace_step <- function(design, step, new_step) {
  rlang::warn(
    "`replace_step()` is deprecated. Reconstruct the design explicitly instead.",
    .frequency = "once", .frequency_id = "replace_step"
  )
  steps <- unclass(design)
  idx <- resolve_step_index(design, step)
  new_label <- attr(new_step, "label") %||% names(steps)[idx]
  steps[[idx]] <- new_step
  names(steps)[idx] <- new_label
  construct_design(steps)
}
