#' Attach diagnosands to a design
#'
#' Stores `diagnosands` as an attribute on `design`; [diagnose_design()] will
#' use these in place of the defaults when present.
#'
#' @param design A `design`.
#' @param diagnosands A diagnosands `design_step`. Defaults to
#'   [default_diagnosands()].
#' @return The design with `diagnosands` attribute set.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N))
#' design <- set_diagnosands(design,
#'   declare_diagnosands(mean_Y = mean(estimate, na.rm = TRUE)))
#' attr(design, "diagnosands") |> attr("step_type")
set_diagnosands <- function(design, diagnosands = default_diagnosands()) {
  attr(design, "diagnosands") <- diagnosands
  design
}

#' The diagnosands `select_diagnosands()` knows about
#'
#' A superset of [default_diagnosands()]. `alpha` and `na.rm` are left as free
#' symbols so that they resolve to the formals of the calling
#' `select_diagnosands()` frame.
#'
#' @keywords internal
#' @noRd
diagnosand_library <- function() {
  list(
    mean_estimand      = rlang::expr(mean(estimand, na.rm = na.rm)),
    mean_estimate      = rlang::expr(mean(estimate, na.rm = na.rm)),
    bias               = rlang::expr(mean(estimate - estimand, na.rm = na.rm)),
    sd_estimate        = rlang::expr(stats::sd(estimate, na.rm = na.rm)),
    rmse               = rlang::expr(sqrt(mean((estimate - estimand)^2, na.rm = na.rm))),
    power              = rlang::expr(mean(p.value <= alpha, na.rm = na.rm)),
    coverage           = rlang::expr(mean(conf.low <= estimand & estimand <= conf.high, na.rm = na.rm)),
    mean_se            = rlang::expr(mean(std.error, na.rm = na.rm)),
    type_s_rate        = rlang::expr(mean((sign(estimate) != sign(estimand))[p.value <= alpha], na.rm = na.rm)),
    exaggeration_ratio = rlang::expr(mean((estimate / estimand)[p.value <= alpha], na.rm = na.rm)),
    var_estimate       = rlang::expr(pop.var(estimate)),
    mean_var_hat       = rlang::expr(mean(std.error^2, na.rm = na.rm)),
    prop_pos_sig       = rlang::expr(mean(estimate > 0 & p.value <= alpha, na.rm = na.rm)),
    mean_ci_length     = rlang::expr(mean(conf.high - conf.low, na.rm = na.rm))
  )
}

#' Build a diagnosands step from a set of named diagnosands
#'
#' Two ways to call this. Naming diagnosands from the built-in library
#' (`select_diagnosands("bias", "power")`) builds a fresh diagnosands step,
#' as in DeclareDesign. Passing an existing diagnosands step first
#' (`select_diagnosands(my_diagnosands, "bias")`) subsets that step instead,
#' which is the way to trim a custom set.
#'
#' The library is a superset of [default_diagnosands()]: `mean_estimand`,
#' `mean_estimate`, `bias`, `sd_estimate`, `rmse`, `power`, `coverage`,
#' `mean_se`, `type_s_rate`, `exaggeration_ratio`, `var_estimate`,
#' `mean_var_hat`, `prop_pos_sig`, and `mean_ci_length`.
#'
#' @param ... Diagnosand names to keep, optionally preceded by a diagnosands
#'   `design_step` to subset.
#' @param alpha Significance level used by `power`, `type_s_rate`,
#'   `exaggeration_ratio`, and `prop_pos_sig`.
#' @param subset An expression evaluated on the simulations table; only the
#'   rows for which it is `TRUE` enter the diagnosands.
#' @param na.rm Passed to the library diagnosands.
#' @return A diagnosands `design_step`.
#' @export
#' @examples
#' select_diagnosands("sd_estimate", "mean_se")
#'
#' # subset a custom set instead of the library
#' select_diagnosands(default_diagnosands(), "bias", "rmse")
select_diagnosands <- function(..., alpha = 0.05, subset = NULL,
                               na.rm = FALSE) {
  args <- list(...)
  base_set <- NULL
  if (length(args) > 0 && inherits(args[[1]], "design_step")) {
    base_set <- args[[1]]
    args <- args[-1]
  }
  keep <- unlist(args, use.names = FALSE)
  if (!is.character(keep) || length(keep) == 0) {
    rlang::abort("Name at least one diagnosand to keep, as a string.")
  }
  if (!is.null(base_set)) return(subset_diagnosands(base_set, keep))
  library <- diagnosand_library()
  unknown <- setdiff(keep, names(library))
  if (length(unknown) > 0) {
    rlang::abort(c(
      paste0("Unknown diagnosand", if (length(unknown) > 1) "s" else "", ": ",
             paste(unknown, collapse = ", "), "."),
      "i" = paste0("Available: ", paste(names(library), collapse = ", "), "."),
      "i" = "Write your own with `declare_diagnosands()`."
    ))
  }
  subset_quo <- rlang::enquo(subset)
  rlang::inject(
    declare_diagnosands(!!!library[keep], subset = !!subset_quo, alpha = alpha)
  )
}

#' Keep a named subset of an existing diagnosands step
#'
#' @keywords internal
#' @noRd
subset_diagnosands <- function(diagnosands, keep) {
  dots <- attr(diagnosands, "dots")
  unknown <- setdiff(keep, names(dots))
  if (length(unknown) > 0) {
    rlang::abort(paste0(
      "Diagnosand", if (length(unknown) > 1) "s" else "", " not in this set: ",
      paste(unknown, collapse = ", "), "."
    ))
  }
  rebuild_step(diagnosands, dots[keep])
}

#' Attach a citation to a design (stub)
#'
#' Records bibliographic information on the design. This is currently a
#' lightweight stub.
#'
#' @param design A `design`.
#' @param ... Citation fields (e.g., `title`, `author`, `year`).
#' @return The design with a `citation` attribute set.
#' @export
#' @examples
#' design <- declare_model(N = 10, Y = rnorm(N))
#' set_citation(design, title = "Example", author = "Coppock", year = 2026)
set_citation <- function(design, ...) {
  attr(design, "citation") <- list(...)
  design
}

#' Retrieve a design's citation (stub)
#'
#' @param design A `design`.
#' @return The citation list, or `NULL` if none has been set.
#' @export
#' @examples
#' design <- set_citation(declare_model(N = 10, Y = rnorm(N)),
#'                        title = "Example")
#' cite_design(design)
cite_design <- function(design) {
  attr(design, "citation")
}
