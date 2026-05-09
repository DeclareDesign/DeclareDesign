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
  # If the handler already returned a tidy data frame, pass it through directly.
  # broom::tidy.data.frame is deprecated (it computes column-level summary stats,
  # not what we want) and will be removed; the right fix is to not call it at all.
  if (is.data.frame(fit)) return(tibble::as_tibble(fit))
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

#' Render a captured `.method` expression as a short character label
#'
#' Strips any namespace prefix so `estimatr::lm_robust` becomes `"lm_robust"`.
#' Anonymous or otherwise un-named methods fall back to `"custom"`.
#'
#' @keywords internal
#' @noRd
method_expr_label <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (is.symbol(expr)) return(as.character(expr))
  if (is.call(expr)) {
    head <- expr[[1]]
    if (is.symbol(head) && as.character(head) %in% c("::", ":::") &&
        length(expr) >= 3L && is.symbol(expr[[3]])) {
      return(as.character(expr[[3]]))
    }
  }
  out <- tryCatch(deparse(expr, width.cutoff = 60L)[[1]], error = function(e) NA_character_)
  if (is.na(out) || nchar(out) > 40L) "custom" else out
}

#' Normalize an `inquiry` argument to a character vector
#'
#' @keywords internal
#' @noRd
normalize_inquiry <- function(inquiry) {
  if (is.null(inquiry)) return(NULL)
  if (is.character(inquiry)) return(inquiry)
  if (inherits(inquiry, "design_step")) {
    rlang::abort(
      "Pass the inquiry label as a string, not a step object.",
      "i" = 'Use `inquiry = "ATE"` instead of `inquiry = my_inquiry_step`.'
    )
  }
  if (is.list(inquiry)) {
    return(unlist(lapply(inquiry, normalize_inquiry)))
  }
  as.character(inquiry)
}

#' Evaluate a list of quosures preserving formulas
#'
#' Splats a named list of quosures into ordinary argument values for
#' `do.call()`. Quosures whose expressions are formulas are returned as
#' formula objects so NSE-using methods (e.g., `lm_robust`, `lm_lin`) see the
#' expected formula on their right-hand side.
#'
#' @keywords internal
#' @noRd
eval_dots <- function(dots, data = NULL) {
  lapply(dots, function(q) {
    e <- rlang::quo_get_expr(q)
    # Detect a `~` call (which `rlang::is_formula()` treats as a formula even
    # before evaluation). Evaluate it in the quosure's environment so the
    # resulting formula object carries that environment with it.
    if (rlang::is_formula(e)) {
      env <- rlang::quo_get_env(q)
      f <- eval(e, envir = env)
      if (is.null(environment(f))) environment(f) <- env
      return(f)
    }
    rlang::eval_tidy(q, data = data)
  })
}

#' Build an estimator/test step closure
#'
#' @keywords internal
#' @noRd
make_estimator_step <- function(method, summary_fn, dots, label, inquiry, term,
                                add_inquiry, handler = NULL) {
  force(method)
  force(summary_fn)
  force(dots)
  force(label)
  force(inquiry)
  force(term)
  force(add_inquiry)
  force(handler)
  # Capture eval_dots by value so the closure is fully self-contained and works
  # inside furrr workers without requiring the DeclareDesignZero namespace.
  ed <- eval_dots
  function(data) {
    if (!is.null(handler)) {
      args <- ed(dots, data = data)
      res <- do.call(handler, c(list(data), args))
    } else {
      args <- ed(dots, data = data)
      fit <- do.call(method, c(args, list(data = data)))
      res <- summary_fn(fit)
    }
    res <- tibble::as_tibble(res)
    if (!"estimator" %in% names(res) || all(is.na(res$estimator))) {
      res$estimator <- label
    }
    # Term filtering. `term = TRUE` (or any non-character truthy) means
    # "return every term"; `term = FALSE` is treated as "no explicit filter".
    # When term is a character vector, preserve the user's ordering so the
    # output rows align with `inquiry =` if both are vectors.
    if ("term" %in% names(res)) {
      if (is.character(term)) {
        ord <- match(term, res$term)
        ord <- ord[!is.na(ord)]
        res <- res[ord, , drop = FALSE]
      } else if (isTRUE(term)) {
        # keep all rows, do not drop (Intercept)
      } else if (nrow(res) > 1L) {
        keep <- res$term != "(Intercept)"
        if (any(keep)) res <- res[keep, , drop = FALSE]
      }
    }
    if (add_inquiry && !is.null(inquiry)) {
      if (!"inquiry" %in% names(res)) {
        if (nrow(res) == length(inquiry) || length(inquiry) == 1L) {
          res$inquiry <- inquiry
        } else if (nrow(res) == 1L && length(inquiry) > 1L) {
          # Replicate the single estimate row once per inquiry so the merge
          # can attach a different estimand to each.
          res <- res[rep(1L, length(inquiry)), , drop = FALSE]
          res$inquiry <- inquiry
        } else {
          res$inquiry <- inquiry[1]
        }
      }
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
#' @param handler Optional handler function. When supplied, the estimator
#'   bypasses `.method`/`.summary` and instead calls
#'   `handler(data, ...evaluated_dots...)`, which must return a tidy table.
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' design <- declare_model(N = 50, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y)) +
#'   declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
#'                     inquiry = "mu", label = "ols")
#' draw_estimates(design)
declare_estimator <- function(..., .method = NULL, .summary = tidy_try,
                              inquiry = NULL, term = NULL, label = "estimator",
                              handler = NULL, draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  method_expr <- substitute(.method)
  if (is.null(.method)) {
    if (requireNamespace("estimatr", quietly = TRUE)) {
      .method <- estimatr::lm_robust
      method_name <- "lm_robust"
    } else {
      .method <- stats::lm
      method_name <- "lm"
    }
  } else {
    method_name <- method_expr_label(method_expr)
  }
  inquiry_chr <- normalize_inquiry(inquiry)
  fn <- make_estimator_step(
    method      = .method,
    summary_fn  = .summary,
    dots        = dots,
    label       = label,
    inquiry     = inquiry_chr,
    term        = term,
    add_inquiry = TRUE,
    handler     = handler
  )
  step <- build_step(
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
    term_arg     = term,
    handler_fn   = handler,
    method_name  = method_name
  )
  attr(step, "draws") <- as.integer(draws)
  step
}

#' Declare a hypothesis test
#'
#' Like [declare_estimator()] but the result table does not gain an `inquiry`
#' column; intended for tests not tied to an estimand.
#'
#' @inheritParams declare_estimator
#' @param handler Optional handler function. When supplied, the test bypasses
#'   `.method`/`.summary` and instead calls `handler(data, ...)` with the
#'   evaluated dots, which must return a tidy table.
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
#'   declare_test(Y ~ Z, .method = lm, term = "Z", label = "diff")
#' draw_estimates(design)
declare_test <- function(..., .method = NULL, .summary = tidy_try,
                         term = NULL, label = "test", handler = NULL,
                         draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  method_expr <- substitute(.method)
  if (is.null(.method)) {
    .method <- stats::lm
    method_name <- "lm"
  } else {
    method_name <- method_expr_label(method_expr)
  }
  fn <- make_estimator_step(
    method      = .method,
    summary_fn  = .summary,
    dots        = dots,
    label       = label,
    inquiry     = NULL,
    term        = term,
    add_inquiry = FALSE,
    handler     = handler
  )
  step <- build_step(
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
    term_arg     = term,
    handler_fn   = handler,
    method_name  = method_name
  )
  attr(step, "draws") <- as.integer(draws)
  step
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
