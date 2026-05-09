#' Tidy a model fit, with a fallback
#'
#' Tries [broom::tidy()] with confidence intervals; if that fails (because the
#' model class has no `tidy` method), assembles a minimal tidy table from the
#' coefficient summary.
#'
#' @param fit A fitted model.
#' @param ... Passed to [broom::tidy()].
#' @return A tibble with columns including `term`, `estimate`, `std.error`,
#'   `statistic`, `p.value`, `conf.low`, `conf.high`.
#' @export
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' tidy_try(fit)
tidy_try <- function(fit, ...) {
  out <- tryCatch(
    broom::tidy(fit, conf.int = TRUE, ...),
    error = function(e) NULL
  )
  if (!is.null(out)) return(tibble::as_tibble(out))
  s <- tryCatch(coef(summary(fit)), error = function(e) NULL)
  if (is.null(s)) {
    return(tibble::tibble(
      term      = character(0),
      estimate  = numeric(0),
      std.error = numeric(0),
      statistic = numeric(0),
      p.value   = numeric(0),
      conf.low  = numeric(0),
      conf.high = numeric(0)
    ))
  }
  cn <- colnames(s)
  est_col <- cn[grepl("Estimate|Value", cn)][1]
  se_col  <- cn[grepl("Std", cn)][1]
  stat_col <- cn[grepl("value$", cn) & cn != est_col][1]
  p_col   <- cn[grepl("^Pr", cn)][1]
  tibble::tibble(
    term      = rownames(s),
    estimate  = if (!is.na(est_col)) s[, est_col] else NA_real_,
    std.error = if (!is.na(se_col)) s[, se_col] else NA_real_,
    statistic = if (!is.na(stat_col)) s[, stat_col] else NA_real_,
    p.value   = if (!is.na(p_col)) s[, p_col] else NA_real_,
    conf.low  = NA_real_,
    conf.high = NA_real_
  )
}

#' Normalize an `inquiry` argument to a character vector
#'
#' @keywords internal
#' @noRd
normalize_inquiry <- function(inquiry) {
  if (is.null(inquiry)) return(NULL)
  if (is.character(inquiry)) return(inquiry)
  if (inherits(inquiry, "design_step")) return(attr(inquiry, "label"))
  if (is.list(inquiry)) {
    return(unlist(lapply(inquiry, normalize_inquiry)))
  }
  as.character(inquiry)
}

#' Build an estimator/test step closure
#'
#' @keywords internal
#' @noRd
make_estimator_step <- function(method, summary_fn, dots, label, inquiry, term,
                                add_inquiry) {
  force(method)
  force(summary_fn)
  force(dots)
  force(label)
  force(inquiry)
  force(term)
  force(add_inquiry)
  function(data) {
    fit <- rlang::inject(method(!!!dots, data = data))
    res <- summary_fn(fit)
    res <- tibble::as_tibble(res)
    res$estimator <- label
    if (add_inquiry && !is.null(inquiry)) {
      if (length(inquiry) == 1L || nrow(res) == length(inquiry)) {
        res$inquiry <- inquiry
      } else {
        res$inquiry <- inquiry[1]
      }
    }
    if (!is.null(term)) {
      res <- res[res$term %in% term, , drop = FALSE]
    }
    res
  }
}

#' Declare an estimator
#'
#' Wraps a model-fitting function with metadata that links its tidied output
#' to one or more inquiries during diagnosis.
#'
#' @param ... Arguments forwarded to `.method`. Typically the formula appears
#'   first (e.g., `Y ~ Z`).
#' @param .method The model-fitting function. Defaults to
#'   [estimatr::lm_robust()].
#' @param .summary Function used to tidy `.method`'s output. Defaults to
#'   [tidy_try()].
#' @param inquiry Either an inquiry label (character), a `design_step`, or a
#'   list of these; used to join the estimate to its target estimand.
#' @param term Optional character vector restricting which model terms appear
#'   in the result.
#' @param label Step label. Defaults to `"estimator"`.
#' @return A `design_step`.
#' @export
#' @examples
#' design <- declare_model(N = 50, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y)) +
#'   declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
#'                     inquiry = "mu", label = "ols")
#' draw_estimates(design)
declare_estimator <- function(..., .method = NULL, .summary = tidy_try,
                              inquiry = NULL, term = NULL, label = "estimator") {
  dots <- rlang::enquos(...)
  call <- sys.call()
  if (is.null(.method)) {
    if (requireNamespace("estimatr", quietly = TRUE)) {
      .method <- estimatr::lm_robust
    } else {
      .method <- stats::lm
    }
  }
  inquiry_chr <- normalize_inquiry(inquiry)
  fn <- make_estimator_step(
    method      = .method,
    summary_fn  = .summary,
    dots        = dots,
    label       = label,
    inquiry     = inquiry_chr,
    term        = term,
    add_inquiry = TRUE
  )
  build_step(
    fn           = fn,
    handler_expr = quote(declare_estimator),
    dots         = dots,
    step_type    = "estimator",
    causal_type  = "estimator",
    label        = label,
    call         = call,
    method_arg   = .method,
    summary_arg  = .summary,
    inquiry_arg  = inquiry_chr,
    term_arg     = term
  )
}

#' Declare a hypothesis test
#'
#' Like [declare_estimator()] but the result table does not gain an `inquiry`
#' column; intended for tests not tied to an estimand.
#'
#' @inheritParams declare_estimator
#' @return A `design_step`.
#' @export
#' @examples
#' design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
#'   declare_test(Y ~ Z, .method = lm, term = "Z", label = "diff")
#' draw_estimates(design)
declare_test <- function(..., .method = NULL, .summary = tidy_try,
                         term = NULL, label = "test") {
  dots <- rlang::enquos(...)
  call <- sys.call()
  if (is.null(.method)) .method <- stats::lm
  fn <- make_estimator_step(
    method      = .method,
    summary_fn  = .summary,
    dots        = dots,
    label       = label,
    inquiry     = NULL,
    term        = term,
    add_inquiry = FALSE
  )
  build_step(
    fn           = fn,
    handler_expr = quote(declare_test),
    dots         = dots,
    step_type    = "test",
    causal_type  = "estimator",
    label        = label,
    call         = call,
    method_arg   = .method,
    summary_arg  = .summary,
    inquiry_arg  = NULL,
    term_arg     = term
  )
}

#' Wrap a custom function as a labeled estimator
#'
#' `label_estimator()` and `label_test()` return functions suitable for use
#' as `.method` in [declare_estimator()] / [declare_test()]; they tag the
#' tidied output with `estimator`, `inquiry`, and a term filter.
#'
#' @param .method A function that fits a model from `data` and `...`.
#' @param label Estimator label.
#' @param inquiry Inquiry label (or list of labels).
#' @param term Optional term filter.
#' @param .summary Function for tidying the model fit. Defaults to [tidy_try()].
#' @return A function suitable for use inside [declare_estimator()].
#' @export
#' @examples
#' my_est <- label_estimator(
#'   function(data, ...) lm(Y ~ Z, data = data),
#'   label = "lm", inquiry = "ATE", term = "Z"
#' )
#' df <- data.frame(Y = rnorm(20), Z = rep(0:1, 10))
#' my_est(df)
label_estimator <- function(.method, label = NULL, inquiry = NULL, term = NULL,
                            .summary = tidy_try) {
  inquiry_chr <- normalize_inquiry(inquiry)
  function(data, ...) {
    fit <- .method(data = data, ...)
    res <- tibble::as_tibble(.summary(fit))
    if (!is.null(label)) res$estimator <- label
    if (!is.null(inquiry_chr)) res$inquiry <- inquiry_chr[1]
    if (!is.null(term)) res <- res[res$term %in% term, , drop = FALSE]
    res
  }
}

#' @rdname label_estimator
#' @export
label_test <- label_estimator
