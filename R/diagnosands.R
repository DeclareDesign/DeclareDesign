#' The stock diagnosand definitions
#'
#' One definition per name, read by every way of asking for a diagnosand:
#' [default_diagnosands()], and a character vector passed to
#' [diagnose_design()]. Writing them once is what keeps `"bias"` from meaning
#' two different things depending on how it was requested.
#'
#' @param alpha Significance level used by `power`, `type_s_rate`,
#'   `exaggeration_ratio`, and `prop_pos_sig`.
#' @param na.rm Whether a simulation that produced `NA` is dropped from a
#'   diagnosand or takes it to `NA`.
#' @return A named list of expressions.
#' @keywords internal
#' @noRd
stock_diagnosand_exprs <- function(alpha = 0.05, na.rm = TRUE) {
  list(
    mean_estimand      = rlang::expr(mean(estimand, na.rm = !!na.rm)),
    mean_estimate      = rlang::expr(mean(estimate, na.rm = !!na.rm)),
    bias               = rlang::expr(mean(estimate - estimand, na.rm = !!na.rm)),
    sd_estimate        = rlang::expr(stats::sd(estimate, na.rm = !!na.rm)),
    rmse               = rlang::expr(sqrt(mean((estimate - estimand)^2, na.rm = !!na.rm))),
    power              = rlang::expr(mean(p.value <= !!alpha, na.rm = !!na.rm)),
    coverage           = rlang::expr(mean(conf.low <= estimand & estimand <= conf.high, na.rm = !!na.rm)),
    mean_se            = rlang::expr(mean(std.error, na.rm = !!na.rm)),
    type_s_rate        = rlang::expr(mean((sign(estimate) != sign(estimand))[p.value <= !!alpha], na.rm = !!na.rm)),
    exaggeration_ratio = rlang::expr(mean((estimate / estimand)[p.value <= !!alpha], na.rm = !!na.rm)),
    var_estimate       = rlang::expr(pop.var(estimate)),
    mean_var_hat       = rlang::expr(mean(std.error^2, na.rm = !!na.rm)),
    prop_pos_sig       = rlang::expr(mean(estimate > 0 & p.value <= !!alpha, na.rm = !!na.rm)),
    mean_ci_length     = rlang::expr(mean(conf.high - conf.low, na.rm = !!na.rm))
  )
}

#' The names of the stock diagnosands
#'
#' @return A character vector.
#' @export
#' @examples
#' stock_diagnosand_names()
stock_diagnosand_names <- function() names(stock_diagnosand_exprs())

#' Which of the stock diagnosands [default_diagnosands()] uses
#'
#' @keywords internal
#' @noRd
default_diagnosand_names <- c("mean_estimand", "mean_estimate", "bias",
                              "sd_estimate", "rmse", "power", "coverage")

#' Construct a diagnosands object
#'
#' @param quos A named list of quosures.
#' @param subset_quo A quosure filtering the simulations, or `NULL`.
#' @keywords internal
#' @noRd
new_diagnosands <- function(quos, subset_quo = NULL) {
  if (is.null(names(quos)) || any(!nzchar(names(quos)))) {
    rlang::abort("Every diagnosand must be named.")
  }
  structure(quos, subset_quo = subset_quo,
            class = c("diagnosands", "dd"))
}

#' The expressions a diagnosands object holds
#'
#' @keywords internal
#' @noRd
diagnosand_dots <- function(x) {
  out <- unclass(x)
  attributes(out) <- list(names = names(out))
  out
}

#' Declare diagnosands
#'
#' Defines summary statistics computed across simulations. Each named
#' expression is evaluated against the simulations data frame, grouped by
#' `inquiry`, `estimator`, `term`, and `outcome` where present.
#'
#' Two diagnosands objects can be joined with `+`, which unions them and lets
#' a name declared twice take its later definition. Adding to the defaults and
#' overriding one of them are therefore the same act:
#'
#' ```
#' default_diagnosands() + declare_diagnosands(mae = mean(abs(estimate - estimand)))
#' default_diagnosands() + declare_diagnosands(power = mean(p.value <= 0.1))
#' ```
#'
#' @param ... Named expressions defining diagnosands.
#' @param subset An expression evaluated on the simulations table; only rows
#'   for which it is `TRUE` enter the diagnosands. `NULL` (the default) keeps
#'   every simulation.
#' @param alpha Significance level. Any diagnosand expression that mentions
#'   `alpha` sees this value.
#' @return A `diagnosands` object.
#' @seealso [default_diagnosands()] for the stock set, and [diagnose_design()],
#'   whose `diagnosands` argument also accepts a character vector of
#'   [stock_diagnosand_names()].
#' @export
#' @examples
#' diags <- declare_diagnosands(
#'   bias = mean(estimate - estimand, na.rm = TRUE),
#'   rmse = sqrt(mean((estimate - estimand)^2, na.rm = TRUE))
#' )
#' names(diags)
#'
#' # power at the 10 percent level
#' declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
declare_diagnosands <- function(..., subset = NULL, alpha = 0.05) {
  new_diagnosands(
    bind_alpha(rlang::enquos(...), alpha),
    subset_quo = unwrap_quosure(rlang::enquo(subset))
  )
}

#' Default diagnosands
#'
#' The standard set: mean estimand, mean estimate, bias, SD of estimates,
#' RMSE, power, and coverage of nominal confidence intervals. Drawn from the
#' same stock definitions a character vector of names reaches, so a name means
#' one thing however it was asked for.
#'
#' @param alpha Significance level used by `power`.
#' @param na.rm Whether a simulation that produced `NA` is dropped from a
#'   diagnosand or takes it to `NA`. `TRUE`, the default, means one failed fit
#'   does not take a whole diagnosand with it.
#' @return A `diagnosands` object.
#' @export
#' @examples
#' default_diagnosands()
#' default_diagnosands(alpha = 0.01)
default_diagnosands <- function(alpha = 0.05, na.rm = TRUE) {
  stock_diagnosands(default_diagnosand_names, alpha = alpha, na.rm = na.rm)
}

#' Build a diagnosands object from stock names
#'
#' @keywords internal
#' @noRd
stock_diagnosands <- function(names, alpha = 0.05, na.rm = TRUE) {
  exprs <- stock_diagnosand_exprs(alpha = alpha, na.rm = na.rm)
  unknown <- setdiff(names, base::names(exprs))
  if (length(unknown) > 0) {
    rlang::abort(c(
      paste0("Unknown diagnosand", if (length(unknown) > 1) "s" else "", ": ",
             paste(unknown, collapse = ", "), "."),
      "i" = paste0("Available: ", paste(base::names(exprs), collapse = ", "), "."),
      "i" = "Write your own with `declare_diagnosands()`."
    ))
  }
  env <- rlang::current_env()
  new_diagnosands(lapply(exprs[names], rlang::new_quosure, env = env))
}

#' Resolve the `diagnosands` argument
#'
#' `NULL` means the defaults, a character vector names stock diagnosands, and
#' a `diagnosands` object is itself.
#'
#' @keywords internal
#' @noRd
as_diagnosands <- function(x) {
  if (is.null(x)) return(default_diagnosands())
  if (inherits(x, "diagnosands")) return(x)
  if (is.character(x)) return(stock_diagnosands(x))
  rlang::abort(c(
    "`diagnosands` must be a diagnosands object or a character vector of names.",
    "i" = 'Names: `diagnose_design(design, diagnosands = c("bias", "rmse"))`.',
    "i" = "Expressions: `declare_diagnosands()`, joined to the defaults with `+`.",
    "i" = paste0("Available names: ",
                 paste(stock_diagnosand_names(), collapse = ", "), ".")
  ))
}

#' Union two diagnosands objects
#'
#' A name declared in both takes its later definition, in the position it
#' already held, so overriding a default does not move it to the end of the
#' diagnosands table.
#'
#' @keywords internal
#' @noRd
union_diagnosands <- function(e1, e2) {
  quos <- diagnosand_dots(e1)
  later <- diagnosand_dots(e2)
  for (nm in names(later)) quos[[nm]] <- later[[nm]]
  new_diagnosands(quos, subset_quo = attr(e2, "subset_quo") %||%
                                       attr(e1, "subset_quo"))
}

#' Print a diagnosands object
#'
#' @param x A `diagnosands` object.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print diagnosands
#' @examples
#' print(default_diagnosands())
print.diagnosands <- function(x, ...) {
  cat(length(x), " diagnosand", if (length(x) == 1) "" else "s", ":\n", sep = "")
  for (nm in names(x)) {
    cat(sprintf("  %s = %s\n", nm, rlang::as_label(rlang::quo_get_expr(x[[nm]]))))
  }
  subset_quo <- attr(x, "subset_quo")
  if (!is.null(subset_quo)) {
    cat(sprintf("  [on simulations where %s]\n",
                rlang::as_label(rlang::quo_get_expr(subset_quo))))
  }
  invisible(x)
}

#' Make `alpha` resolve to the declared value inside diagnosand expressions
#'
#' A diagnosand quosure carries the environment it was written in, so a bare
#' `alpha` would otherwise resolve to whatever the caller happens to have
#' bound, not to the `alpha` argument. Any quosure mentioning the symbol gets
#' a cloned environment with the declared value bound in it.
#'
#' @keywords internal
#' @noRd
bind_alpha <- function(dots, alpha) {
  purrr::map(dots, function(q) {
    expr <- rlang::quo_get_expr(q)
    if (!expr_has_symbol(expr, "alpha")) return(q)
    env <- rlang::env_clone(rlang::quo_get_env(q))
    rlang::env_bind(env, alpha = alpha)
    rlang::new_quosure(expr, env = env)
  })
}

#' Unwrap a quosure that was injected into a quosure-capturing argument
#'
#' Returns `NULL` for an absent subset.
#'
#' @keywords internal
#' @noRd
unwrap_quosure <- function(quo) {
  inner <- rlang::quo_get_expr(quo)
  if (rlang::is_quosure(inner)) quo <- inner
  if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) return(NULL)
  quo
}
