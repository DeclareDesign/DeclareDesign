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
#' If `dots` includes a named `handler` argument that resolves to a function,
#' the step calls `handler(...other_dots..., data = data)` (or omits `data`
#' when the handler does not accept it) instead of going through fabricate.
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
    is_handler <- !is.na(nm) & nm == "handler"
    user_data_quo <- if (any(is_data)) dots[[which(is_data)[1]]] else NULL
    user_handler_quo <- if (any(is_handler)) dots[[which(is_handler)[1]]] else NULL
    rest <- dots[!is_data & !is_handler]
    if (!is.null(user_data_quo)) {
      user_data <- rlang::eval_tidy(user_data_quo)
      if (is.null(data) || (is.data.frame(data) && nrow(data) == 0L)) {
        data <- user_data
      }
    }
    if (!is.null(user_handler_quo)) {
      handler_fn <- rlang::eval_tidy(user_handler_quo)
      if (handler_is_fabricate(handler_fn)) {
        return(rlang::inject(handler_fn(data = data, !!!rest)))
      }
      # Arguments reach the handler as written, which is what `declare_step()`
      # does and what DeclareDesign 1.x did here. A handler that resolves its
      # own arguments against the data (`dplyr::mutate()`, `tidyr::uncount()`,
      # anything using tidyselect or a data mask) needs the expression rather
      # than its value. Evaluating first both broke `n()` and left `uncount()`
      # unable to drop the weights column it was given.
      decl_env <- dots_env(rest, default = rlang::caller_env())
      args <- dots_as_written(rest)
      call_env <- rlang::env(decl_env, .dd_data = data)
      # A handler with no slot for the data builds the table from scratch, so
      # it is called with the declared arguments alone.
      formal_names <- tryCatch(names(formals(handler_fn)),
                               error = function(e) NULL)
      takes_data <- is.null(formal_names) ||
        any(c("data", ".data") %in% formal_names)
      cl <- if (takes_data) {
        rlang::call2(handler_fn, quote(.dd_data), !!!args)
      } else {
        rlang::call2(handler_fn, !!!args)
      }
      return(eval(cl, envir = call_env))
    }
    # Use fabricate_with_dots to avoid double-quoting: !!!-injection turns
    # quosures into formula objects (~expr), which fabricate()'s enquos()
    # would re-capture incorrectly.
    fabricatr:::fabricate_with_dots(data = data, dots = rest)
  }
}

#' Declare the data-generating model
#'
#' The model describes the world the design runs in. It does not have to be
#' correct: declaring one you doubt is how you find out whether the design
#' still works when you are wrong about it.
#'
#' Declares a step that builds (or augments) the population data via
#' [fabricatr::fabricate()]. The first model step in a design receives
#' `data = NULL`, so it must specify `N` (and any variables) directly.
#' Subsequent model steps add columns to the existing data.
#'
#' @family design declarations
#' @param ... Named arguments forwarded to [fabricatr::fabricate()]. Unquoted
#'   expressions are evaluated lazily in the caller's environment with access
#'   to the current data frame.
#' @param label Step label. Defaults to `"model"`.
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' design <- declare_model(N = 50, U = rnorm(N), Y = U + 1)
#'
#' head(draw_data(design))
#'
#' # A second model step adds columns to the data the first one made.
#' design <-
#'   declare_model(N = 50, U = rnorm(N)) +
#'   declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.5)
#'
#' head(draw_data(design))
declare_model <- function(..., label = "model", draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
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
#' Records what the study observes: the measured outcome, typically `Y`
#' revealed from the potential outcomes given the assignment `Z`. Columns are
#' added to the data the way [declare_model()] adds them, without a new ID
#' column.
#'
#' @inheritParams declare_model
#' @family design declarations
#' @return A `design_step`.
#' @export
#' @examples
#' design <-
#'   declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
#'   declare_assignment(Z = sample(rep(0:1, length.out = N))) +
#'   declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z)
#'
#' head(draw_data(design))
declare_measurement <- function(..., label = "measurement", draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
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
#' Assigns units to treatment conditions, adding the assignment variable (`Z`
#' by convention) to the data. The randomization itself is usually a randomizr
#' function such as [randomizr::complete_ra()].
#'
#' @inheritParams declare_model
#' @family design declarations
#' @return A `design_step`.
#' @export
#' @examples
#' design <-
#'   declare_model(N = 100, U = rnorm(N)) +
#'   declare_assignment(Z = sample(rep(0:1, length.out = N)))
#'
#' table(draw_data(design)$Z)
declare_assignment <- function(..., label = "assignment", draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
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
    # A custom handler is the whole sampling step, as in 1.x: it receives the
    # data and returns the sample, and only an explicit `filter` is applied
    # on top of it.
    if (any(!is.na(nm) & nm == "handler")) {
      data <- make_fabricate_step(dots, id_label_na = TRUE)(data)
      if (!is.null(filter_quo)) {
        keep <- rlang::eval_tidy(filter_quo, data = data)
        data <- data[!is.na(keep) & keep, , drop = FALSE]
      }
      return(data)
    }
    is_data <- !is.na(nm) & nm == "data"
    user_data_quo <- if (any(is_data)) dots[[which(is_data)[1]]] else NULL
    rest <- dots[!is_data]
    if (!is.null(user_data_quo)) {
      user_data <- rlang::eval_tidy(user_data_quo)
      if (is.null(data) || (is.data.frame(data) && nrow(data) == 0L)) {
        data <- user_data
      }
    }
    data <- fabricatr:::fabricate_with_dots(data = data, dots = rest)
    if (!is.null(filter_quo)) {
      keep <- rlang::eval_tidy(filter_quo, data = data)
      data <- data[!is.na(keep) & keep, , drop = FALSE]
    } else if ("S" %in% names(data)) {
      keep <- data[["S"]]
      data <- data[!is.na(keep) & keep == 1, , drop = FALSE]
    } else {
      rlang::warn(c(
        "`declare_sampling()` found no column named `S` and no `filter`, so every row was kept.",
        i = paste0("Name the sampling indicator `S`, as in `declare_sampling(S = ",
                   "complete_rs(N, n = 50))`, or say which rows to keep with `filter = `.")
      ), .frequency = "once", .frequency_id = "dd_sampling_no_S")
    }
    data
  }
}

#' Declare a sampling procedure
#'
#' Draws the sample from the population, keeping the rows where the sampling
#' indicator `S` is 1. Name the indicator `S`, as in
#' `declare_sampling(S = complete_rs(N, n = 50))`, or say which rows to keep
#' with `filter = `; a sampling step that produces neither keeps every row and
#' warns.
#'
#' @inheritParams declare_model
#' @family design declarations
#' @param filter Optional unquoted expression evaluated against the data;
#'   rows where the expression is `TRUE` are retained.
#' @return A `design_step`.
#' @export
#' @examples
#' design <-
#'   declare_model(N = 200, Y = rnorm(N)) +
#'   declare_sampling(S = sample(rep(0:1, length.out = N)))
#'
#' nrow(draw_data(design))
declare_sampling <- function(..., filter = NULL, label = "sampling",
                             draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
  filter_quo <- capture_quosure_env(rlang::enquo(filter))
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
