#' Build a DGP step closure
#'
#' Helper that splices a captured set of quosures into a `fabricatr::fabricate`
#' call. Pulled out into its own helper so that [redesign()] can re-create
#' steps after rebinding parameters in the captured environment.
#'
#' If the captured `dots` include a named `data` argument, that data frame is
#' used as the starting state instead of (or in addition to) what is threaded
#' in by the design.
#'
#' @param dots Named list of quosures.
#' @param id_label_na Logical; if `TRUE`, pass `ID_label = NA` so fabricate
#'   does not append a row id (used by measurement, assignment, sampling).
#' @return A function of signature `function(data = NULL)`.
#' @keywords internal
#' @noRd
make_fabricate_step <- function(dots, id_label_na = FALSE) {
  force(dots)
  force(id_label_na)
  function(data = NULL) {
    nm <- names(dots) %||% rep("", length(dots))
    is_data <- !is.na(nm) & nm == "data"
    user_data_quo <- if (any(is_data)) dots[[which(is_data)[1]]] else NULL
    rest <- dots[!is_data]
    if (!is.null(user_data_quo)) {
      user_data <- rlang::eval_tidy(user_data_quo)
      if (is.null(data) || (is.data.frame(data) && nrow(data) == 0L)) {
        data <- user_data
      }
    }
    if (id_label_na) {
      rlang::inject(fabricatr::fabricate(data = data, !!!rest, ID_label = NA))
    } else {
      rlang::inject(fabricatr::fabricate(data = data, !!!rest))
    }
  }
}

#' Declare the data-generating model
#'
#' Declares a step that builds (or augments) the population data via
#' [fabricatr::fabricate()]. The first model step in a design receives
#' `data = NULL`, so it must specify `N` (and any variables) directly.
#' Subsequent model steps add columns to the existing data.
#'
#' @param ... Named arguments forwarded to [fabricatr::fabricate()]. Unquoted
#'   expressions are evaluated lazily in the caller's environment with access
#'   to the current data frame.
#' @param label Step label. Defaults to `"model"`.
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_model(N = 50, U = rnorm(N), Y = U + 1)
#' df <- step(NULL)
#' nrow(df)
declare_model <- function(..., label = "model", draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  fn <- make_fabricate_step(dots, id_label_na = FALSE)
  step <- build_step(
    fn          = fn,
    handler_expr = quote(fabricatr::fabricate),
    dots        = dots,
    step_type   = "model",
    causal_type = "dgp",
    label       = label,
    call        = call
  )
  attr(step, "draws") <- as.integer(draws)
  step
}

#' Declare measurement
#'
#' Like [declare_model()] but does not append an ID column; intended for
#' creating outcome measurements after assignment.
#'
#' @inheritParams declare_model
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z)
#' attr(step, "step_type")
declare_measurement <- function(..., label = "measurement", draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  fn <- make_fabricate_step(dots, id_label_na = TRUE)
  step <- build_step(
    fn          = fn,
    handler_expr = quote(fabricatr::fabricate),
    dots        = dots,
    step_type   = "measurement",
    causal_type = "dgp",
    label       = label,
    call        = call
  )
  attr(step, "draws") <- as.integer(draws)
  step
}

#' Declare an assignment procedure
#'
#' Augments the data with an assignment column (typically using
#' [randomizr::complete_ra()] or similar).
#'
#' @inheritParams declare_model
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_assignment(Z = sample(rep(0:1, length.out = N)))
#' attr(step, "step_type")
declare_assignment <- function(..., label = "assignment", draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  fn <- make_fabricate_step(dots, id_label_na = TRUE)
  step <- build_step(
    fn          = fn,
    handler_expr = quote(fabricatr::fabricate),
    dots        = dots,
    step_type   = "assignment",
    causal_type = "dgp",
    label       = label,
    call        = call
  )
  attr(step, "draws") <- as.integer(draws)
  step
}

#' Build a sampling step closure
#'
#' @keywords internal
#' @noRd
make_sampling_step <- function(dots, filter_quo) {
  force(dots)
  force(filter_quo)
  function(data = NULL) {
    nm <- names(dots) %||% rep("", length(dots))
    is_data <- !is.na(nm) & nm == "data"
    user_data_quo <- if (any(is_data)) dots[[which(is_data)[1]]] else NULL
    rest <- dots[!is_data]
    if (!is.null(user_data_quo)) {
      user_data <- rlang::eval_tidy(user_data_quo)
      if (is.null(data) || (is.data.frame(data) && nrow(data) == 0L)) {
        data <- user_data
      }
    }
    data <- rlang::inject(
      fabricatr::fabricate(data = data, !!!rest, ID_label = NA)
    )
    if (!is.null(filter_quo)) {
      keep <- rlang::eval_tidy(filter_quo, data = data)
      data <- data[!is.na(keep) & keep, , drop = FALSE]
    } else if ("S" %in% names(data)) {
      keep <- data[["S"]]
      data <- data[!is.na(keep) & keep == 1, , drop = FALSE]
    }
    data
  }
}

#' Declare a sampling procedure
#'
#' Adds a sampling indicator and subsets to sampled rows. By default, if no
#' `filter` is supplied and an `S` column is produced, rows with `S == 1` are
#' kept.
#'
#' @inheritParams declare_model
#' @param filter Optional unquoted expression evaluated against the data;
#'   rows where the expression is `TRUE` are retained.
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_sampling(S = sample(rep(0:1, length.out = N)))
#' attr(step, "step_type")
declare_sampling <- function(..., filter = NULL, label = "sampling",
                             draws = 1L) {
  dots <- rlang::enquos(...)
  filter_quo <- rlang::enquo(filter)
  if (rlang::quo_is_null(filter_quo)) filter_quo <- NULL
  call <- sys.call()
  fn <- make_sampling_step(dots, filter_quo)
  step <- build_step(
    fn           = fn,
    handler_expr = quote(fabricatr::fabricate),
    dots         = dots,
    step_type    = "sampling",
    causal_type  = "dgp",
    label        = label,
    call         = call,
    filter_quo   = filter_quo
  )
  attr(step, "draws") <- as.integer(draws)
  step
}
