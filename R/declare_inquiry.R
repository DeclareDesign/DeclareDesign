#' Build an inquiry step closure
#'
#' @keywords internal
#' @noRd
make_inquiry_step <- function(dots, subset_quo, label, handler = NULL) {
  force(dots)
  force(subset_quo)
  force(label)
  force(handler)
  function(data) {
    if (!is.null(subset_quo)) {
      keep <- rlang::eval_tidy(subset_quo, data = data)
      data <- data[!is.na(keep) & keep, , drop = FALSE]
    }
    if (!is.null(handler)) {
      # Evaluate left-to-right so named outputs from earlier args are visible
      # to later ones (mirroring the no-handler branch). This lets users write
      # patterns like `X = x_range, inquiry = str_c("X_", X)`.
      eval_env <- if (is.data.frame(data)) as.list(data) else list()
      arg_names <- names(dots) %||% rep("", length(dots))
      args <- vector("list", length(dots))
      names(args) <- arg_names
      for (i in seq_along(dots)) {
        q <- dots[[i]]
        nm <- arg_names[i]
        val <- rlang::eval_tidy(q, data = eval_env)
        args[[i]] <- val
        if (!is.null(nm) && nzchar(nm)) eval_env[[nm]] <- val
      }
      # Forward the step `label` to the handler when it accepts a `label` formal
      # and the user has not bound it explicitly via `...`.
      handler_args <- tryCatch(names(formals(handler)), error = function(e) NULL)
      pass_data <- "data" %in% handler_args
      pass_label <- !is.null(handler_args) &&
        "label" %in% handler_args && !"label" %in% names(args)
      call_args <- args
      if (pass_data && !"data" %in% names(call_args)) {
        call_args <- c(list(data = data), call_args)
      }
      if (pass_label) call_args$label <- label
      result <- do.call(handler, call_args)
      result <- tibble::as_tibble(result)
      if (!"inquiry" %in% names(result)) {
        result$inquiry <- label
      }
      if (!"estimand" %in% names(result)) {
        if (ncol(result) >= 1L) {
          first_num <- which(vapply(result, is.numeric, logical(1)))[1]
          if (!is.na(first_num)) {
            result$estimand <- result[[first_num]]
          }
        }
      }
      return(tibble::as_tibble(result))
    }
    nms <- names(dots)
    if (is.null(nms)) nms <- rep("", length(dots))
    if (any(nms == "")) {
      missing <- which(nms == "")
      # Single unnamed inquiry: the inquiry name is the step's label.
      # Multiple unnamed inquiries: disambiguate with a numeric suffix.
      if (length(dots) == 1L) {
        nms[missing] <- label
      } else {
        nms[missing] <- paste0(label, "_", missing)
      }
      names(dots) <- nms
    }
    rows <- list()
    eval_env <- as.list(data)
    for (i in seq_along(dots)) {
      q <- dots[[i]]
      nm <- names(dots)[i]
      val <- rlang::eval_tidy(q, data = eval_env)
      rows[[i]] <- tibble::tibble(inquiry = nm, estimand = val)
      eval_env[[nm]] <- val
    }
    dplyr::bind_rows(rows)
  }
}

#' Declare an inquiry
#'
#' Each named argument records an inquiry: an expression evaluated on the data
#' as it stands when the step runs. Its value on a given draw is the estimand.
#'
#' @family design declarations
#'
#' @param ... Named expressions. Each expression becomes an inquiry whose
#'   numeric value is recorded in the simulation output.
#' @param subset Optional unquoted expression filtering the data before
#'   estimands are computed.
#' @param label Step label. Defaults to `"inquiry"`.
#' @param handler Optional alternative handler. When supplied, `...` arguments
#'   are evaluated against the data and passed to `handler()` rather than
#'   being treated as named scalar inquiries; useful for vectorised inquiry
#'   sets (for example, `handler = tibble`).
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' design <-
#'   declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' draw_estimands(design)
#'
#' # Placed before declare_sampling(), the question is about the population;
#' # placed after, about the sample.
#' design <-
#'   declare_model(N = 500, Y = rnorm(N)) +
#'   declare_inquiry(population_mean = mean(Y)) +
#'   declare_sampling(S = sample(rep(0:1, length.out = N))) +
#'   declare_inquiry(sample_mean = mean(Y))
#'
#' draw_estimands(design)
declare_inquiry <- function(..., subset = NULL, label = "inquiry",
                            handler = NULL, draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
  subset_quo <- rlang::enquo(subset)
  if (rlang::quo_is_null(subset_quo)) subset_quo <- NULL
  call <- sys.call()
  # When the user supplies exactly one named splat (e.g. `pate = mean(...)`),
  # the splat name becomes the step's label so it doubles as a reference key
  # in `declare_estimator(..., inquiry = pate_step)`. An explicit `label =`
  # argument is overridden in that case (DD compatibility).
  splat_names <- names(dots) %||% rep("", length(dots))
  named_splats <- splat_names[nzchar(splat_names)]
  if (length(dots) == 1L && length(named_splats) == 1L) {
    label <- named_splats
  }
  fn <- make_inquiry_step(dots, subset_quo, label, handler = handler)
  step <- build_step(
    fn          = fn,
    handler_expr = quote(declare_inquiry),
    dots        = dots,
    step_type   = "inquiry",
    causal_type = "inquiry",
    label       = label,
    call        = call,
    subset_quo  = subset_quo,
    handler_fn  = handler
  )
  attr(step, "draws") <- as.integer(draws)
  step
}

#' @rdname declare_inquiry
#' @export
declare_inquiries <- declare_inquiry

#' @rdname declare_inquiry
#' @export
declare_estimand <- declare_inquiry

#' @rdname declare_inquiry
#' @export
declare_estimands <- declare_inquiry
