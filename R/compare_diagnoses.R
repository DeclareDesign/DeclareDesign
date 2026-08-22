#' Diagnose an argument that may already be a diagnosis
#'
#' @keywords internal
#' @noRd
as_diagnosis <- function(x, sims, bootstrap_sims, diagnosands) {
  if (inherits(x, "diagnosis")) return(x)
  if (!inherits(x, "design")) {
    stop("Each argument must be a `design` or a `diagnosis` object.")
  }
  diagnose_design(x, sims = sims, bootstrap_sims = bootstrap_sims,
                  diagnosands = diagnosands)
}

#' Put a diagnosis's point estimates in long form
#'
#' @keywords internal
#' @noRd
long_diagnosands <- function(diagnosis, diagnosands, label_cols, keys, suffix) {
  df <- diagnosis$diagnosands_df
  purrr::map(diagnosands, function(d) {
    se_col <- paste0("se(", d, ")")
    dplyr::bind_cols(
      df[, label_cols, drop = FALSE],
      tibble::tibble(
        diagnosand = d,
        mean       = df[[d]],
        se         = if (se_col %in% names(df)) df[[se_col]] else NA_real_
      )
    )
  }) |>
    dplyr::bind_rows() |>
    suffix_columns(keep = c(keys, "diagnosand"), suffix = suffix)
}

#' Suffix the columns that belong to one diagnosis rather than to both
#'
#' The columns rows are matched on are shared and keep their names. Everything
#' else gets `_1` or `_2`, so a column only one of the two designs carries,
#' such as a redesign parameter, still says which design it came from.
#'
#' @keywords internal
#' @noRd
suffix_columns <- function(df, keep, suffix) {
  rename <- setdiff(names(df), keep)
  names(df)[match(rename, names(df))] <- paste0(rename, suffix)
  df
}

#' Put a diagnosis's bootstrap replicates in long form
#'
#' @keywords internal
#' @noRd
long_replicates <- function(diagnosis, diagnosands, label_cols, keys, suffix) {
  df <- diagnosis$bootstrap_replicates
  if (is.null(df)) return(NULL)
  tidyr::pivot_longer(
    df[, c(label_cols, "bootstrap_id", diagnosands), drop = FALSE],
    cols      = dplyr::all_of(diagnosands),
    names_to  = "diagnosand",
    values_to = "value"
  ) |>
    suffix_columns(keep = c(keys, "diagnosand", "bootstrap_id"),
                   suffix = suffix)
}

#' Compare the diagnosands of two designs
#'
#' Diagnoses both designs and reports, for every diagnosand they have in
#' common, the value under each design and the difference between them. The
#' bootstrap replicates behind each diagnosis supply a standard error and a
#' confidence interval for the difference.
#'
#' Rows are matched on whichever of `inquiry`, `estimator`, and `term` both
#' diagnoses carry. With `merge_by_estimator = FALSE`, estimators are not
#' matched to each other and every pair within an inquiry is compared, which
#' is how to compare two estimators that carry different labels.
#'
#' `mean_difference` is the difference of the two point estimates. The
#' bootstrap enters only through `se_difference` and the interval. The two
#' designs are simulated independently, so the replicates are paired
#' arbitrarily; the interval is a percentile interval on that difference.
#'
#' Either argument may be a design or an already computed `diagnosis`, so a
#' diagnosis that took a long time to run can be reused. Passing two diagnoses
#' computed with different diagnosands compares whatever they have in common.
#'
#' @family simulation and diagnosis
#' @param design1,design2 A `design` or a `diagnosis`.
#' @param sims Number of simulations, used only for arguments that are designs.
#' @param bootstrap_sims Number of bootstrap replicates, used only for
#'   arguments that are designs. With `0`, differences are reported without
#'   standard errors or intervals.
#' @param diagnosands A diagnosands `design_step` applied to both designs, so
#'   the two sides are compared on the same footing by construction. Used only
#'   for arguments that are designs; a diagnosis passed in keeps the
#'   diagnosands it was computed with.
#' @param merge_by_estimator Match estimators by label. `FALSE` compares every
#'   pair of estimators within an inquiry.
#' @param alpha One minus the coverage of the reported interval.
#' @return A `compared_diagnoses` object: a list with `compared_diagnoses_df`
#'   and the two diagnoses.
#' @export
#' @examples
#' design <- declare_parameters(n_units = 100) +
#'   declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = randomizr::complete_ra(N)) +
#'   declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' bigger <- redesign(design, n_units = 200)
#' compare_diagnoses(design, bigger, sims = 20, bootstrap_sims = 20)
compare_diagnoses <- function(design1, design2, sims = 500,
                              bootstrap_sims = 100, diagnosands = NULL,
                              merge_by_estimator = TRUE, alpha = 0.05) {
  diagnosis1 <- as_diagnosis(design1, sims, bootstrap_sims, diagnosands)
  diagnosis2 <- as_diagnosis(design2, sims, bootstrap_sims, diagnosands)

  diagnosands <- intersect(diagnosis1$diagnosand_names,
                           diagnosis2$diagnosand_names)
  if (length(diagnosands) == 0) {
    stop("The two diagnoses have no diagnosands in common.")
  }

  # Every grouping column of each diagnosis is carried into the output, so a
  # redesign parameter that only one of the two has still shows up.
  carry1 <- setdiff(diagnosis1$group_by_set, "design")
  carry2 <- setdiff(diagnosis2$group_by_set, "design")
  key_cols <- intersect(c("inquiry", "estimator", "outcome", "term"),
                        intersect(carry1, carry2))
  if (!merge_by_estimator) key_cols <- setdiff(key_cols, "estimator")
  if (length(key_cols) == 0) {
    stop("The two diagnoses share no inquiry, estimator, outcome, or term ",
         "labels to match rows on.")
  }

  points <- dplyr::inner_join(
    long_diagnosands(diagnosis1, diagnosands, carry1, key_cols, "_1"),
    long_diagnosands(diagnosis2, diagnosands, carry2, key_cols, "_2"),
    by = c(key_cols, "diagnosand"), relationship = "many-to-many"
  )
  if (nrow(points) == 0) {
    stop("The two diagnoses have no labels in common, so no rows could be ",
         "matched.")
  }
  points <- dplyr::mutate(points, mean_difference = .data$mean_2 - .data$mean_1)

  reps1 <- long_replicates(diagnosis1, diagnosands, carry1, key_cols, "_1")
  reps2 <- long_replicates(diagnosis2, diagnosands, carry2, key_cols, "_2")
  if (!is.null(reps1) && !is.null(reps2)) {
    joined <- dplyr::inner_join(
      reps1, reps2,
      by = c(key_cols, "bootstrap_id", "diagnosand"),
      relationship = "many-to-many"
    )
    group_cols <- setdiff(names(joined),
                          c("bootstrap_id", "value_1", "value_2"))
    differences <- joined |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarize(
        se_difference = stats::sd(.data$value_2 - .data$value_1, na.rm = TRUE),
        conf.low  = unname(stats::quantile(.data$value_2 - .data$value_1,
                                           alpha / 2, na.rm = TRUE)),
        conf.high = unname(stats::quantile(.data$value_2 - .data$value_1,
                                           1 - alpha / 2, na.rm = TRUE)),
        .groups = "drop"
      )
    points <- dplyr::left_join(points, differences,
                               by = intersect(names(points), names(differences)))
  } else {
    points$se_difference <- NA_real_
    points$conf.low  <- NA_real_
    points$conf.high <- NA_real_
  }

  out <- dplyr::relocate(
    points,
    dplyr::any_of(c("diagnosand", "mean_1", "mean_2", "mean_difference",
                    "se_1", "se_2", "se_difference", "conf.low", "conf.high")),
    .after = dplyr::last_col()
  )
  structure(
    list(compared_diagnoses_df = out,
         diagnosis1 = diagnosis1,
         diagnosis2 = diagnosis2),
    alpha = alpha,
    class = "compared_diagnoses"
  )
}

#' Print a diagnosand comparison
#'
#' @param x A `compared_diagnoses` object.
#' @param ... Passed to the data frame print method.
#' @return The input invisibly.
#' @export
#' @method print compared_diagnoses
#' @examples
#' design <- declare_parameters(n_units = 100) +
#'   declare_model(N = n_units, Y = rnorm(N), Z = randomizr::complete_ra(N)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' print(compare_diagnoses(design, redesign(design, n_units = 200),
#'                         sims = 20, bootstrap_sims = 20))
print.compared_diagnoses <- function(x, ...) {
  cat("Comparison of diagnosands, design 2 minus design 1\n\n")
  print(x$compared_diagnoses_df, ...)
  invisible(x)
}
