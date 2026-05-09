#' Print a design
#'
#' @param x A `design`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print design
#' @examples
#' d <- declare_model(N = 30, Y = rnorm(N)) + NULL
#' print(d)
print.design <- function(x, ...) {
  cat("Research design with ", length(x), " step(s):\n", sep = "")
  for (nm in names(x)) {
    step <- x[[nm]]
    cat(sprintf("  [%s] %s (%s)\n",
                attr(step, "step_type") %||% "?",
                nm,
                attr(step, "causal_type") %||% "?"))
  }
  invisible(x)
}

#' Print a design step
#'
#' @param x A `design_step`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print design_step
#' @examples
#' s <- declare_model(N = 10, Y = rnorm(N))
#' print(s)
print.design_step <- function(x, ...) {
  cat(sprintf("<design_step: %s [%s]>\n",
              attr(x, "label") %||% "?",
              attr(x, "step_type") %||% "?"))
  invisible(x)
}

#' Print a diagnosis
#'
#' @param x A `diagnosis`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' print(d)
print.diagnosis <- function(x, ...) {
  cat("Research design diagnosis\n")
  sims_df <- x$simulations_df
  if (!is.null(sims_df) && "sim_ID" %in% names(sims_df) && nrow(sims_df) > 0) {
    n <- suppressWarnings(max(sims_df$sim_ID, na.rm = TRUE))
    if (is.finite(n)) {
      msg <- sprintf("  %d simulations", n)
      if (!is.null(x$variance_decomposition)) {
        draw_levels <- x$variance_decomposition$draw_levels[1]
        msg <- paste0(msg, sprintf(" [nested: %s]", draw_levels))
      }
      cat(msg, "\n", sep = "")
    }
  }
  cat("\n")
  cat("Diagnosands:\n")
  print(x$diagnosands_df, ...)
  if (!is.null(x$variance_decomposition)) {
    cat("\nVariance decomposition:\n")
    vd <- dplyr::select(x$variance_decomposition,
                        -dplyr::any_of(c("n_sims", "draw_levels")))
    print(vd, ...)
  }
  invisible(x)
}

#' Summarize a design
#'
#' @param object A `design`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method summary design
#' @examples
#' summary(declare_model(N = 30, Y = rnorm(N)) + NULL)
summary.design <- function(object, ...) {
  print(object)
  invisible(object)
}

#' Tidy a diagnosis
#'
#' Returns the diagnosands table in long form, one row per diagnosand.
#'
#' @param x A `diagnosis`.
#' @param conf.int Reserved.
#' @param ... Reserved.
#' @return A tibble.
#' @importFrom generics tidy
#' @export tidy.diagnosis
#' @export
#' @method tidy diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' tidy.diagnosis(d)
tidy.diagnosis <- function(x, conf.int = FALSE, ...) {
  df <- x$diagnosands_df
  diag_names <- intersect(x$diagnosand_names, names(df))
  if (length(diag_names) == 0) return(tibble::as_tibble(df))
  tidyr::pivot_longer(
    df,
    cols      = dplyr::all_of(diag_names),
    names_to  = "diagnosand",
    values_to = "estimate"
  )
}
