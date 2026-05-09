#' Build an inquiry step closure
#'
#' @keywords internal
#' @noRd
make_inquiry_step <- function(dots, subset_quo, label) {
  force(dots)
  force(subset_quo)
  force(label)
  function(data) {
    if (!is.null(subset_quo)) {
      keep <- rlang::eval_tidy(subset_quo, data = data)
      data <- data[!is.na(keep) & keep, , drop = FALSE]
    }
    nms <- names(dots)
    if (is.null(nms) || any(nms == "")) {
      missing <- which(is.null(nms) | nms == "")
      nms[missing] <- paste0(label, "_", missing)
      names(dots) <- nms
    }
    out <- purrr::imap(dots, function(q, nm) {
      val <- rlang::eval_tidy(q, data = as.list(data))
      tibble::tibble(inquiry = nm, estimand = val)
    })
    dplyr::bind_rows(out)
  }
}

#' Declare an inquiry (estimand)
#'
#' Each named argument defines an inquiry: an expression evaluated on the
#' realized data that yields a numeric estimand.
#'
#' @param ... Named expressions. Each expression becomes an inquiry whose
#'   numeric value is recorded in the simulation output.
#' @param subset Optional unquoted expression filtering the data before
#'   estimands are computed.
#' @param label Step label. Defaults to `"inquiry"`.
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#' attr(step, "step_type")
declare_inquiry <- function(..., subset = NULL, label = "inquiry") {
  dots <- rlang::enquos(...)
  subset_quo <- rlang::enquo(subset)
  if (rlang::quo_is_null(subset_quo)) subset_quo <- NULL
  call <- sys.call()
  fn <- make_inquiry_step(dots, subset_quo, label)
  build_step(
    fn          = fn,
    handler_expr = quote(declare_inquiry),
    dots        = dots,
    step_type   = "inquiry",
    causal_type = "inquiry",
    label       = label,
    call        = call,
    subset_quo  = subset_quo
  )
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
