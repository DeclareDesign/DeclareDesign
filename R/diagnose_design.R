#' Join estimates to their inquiries
#'
#' Used in [run_design()] and [simulate_design()] to attach estimands to
#' estimator output via the `inquiry` label.
#'
#' The join is on every column the two tables share, so a labelled estimator
#' matches on `inquiry`, a conjoint matches on the `attribute` and `level`
#' columns both of its tables carry, and an estimator declared without
#' `inquiry =` still matches on `sim_ID` alone. That last case is what makes
#' the one-inquiry, one-estimator design work without the label: the
#' simulation's only inquiry is the one attached.
#'
#' When the join produces more rows than either table had, the match was
#' genuinely ambiguous and a warning names the columns it went on.
#'
#' The columns the join went on are recorded on the result as the `matched_on`
#' attribute, which [simulate_design()] carries to the simulations table and
#' [print.diagnosis()] reports when the match was anything other than a
#' labelled one.
#'
#' @param estimates A tibble of estimator output.
#' @param inquiries A tibble of inquiry output.
#' @return A tibble, carrying a `matched_on` attribute.
#' @export
#' @keywords internal
merge_estimates_inquiries <- function(estimates, inquiries) {
  if (nrow(estimates) == 0 && nrow(inquiries) == 0) return(tibble::tibble())
  if (nrow(estimates) == 0) return(tibble::as_tibble(inquiries))
  if (nrow(inquiries) == 0) return(tibble::as_tibble(estimates))
  estimates <- tibble::as_tibble(estimates)
  inquiries <- tibble::as_tibble(inquiries)
  shared <- intersect(names(estimates), names(inquiries))
  result <- if (length(shared) == 0) {
    dplyr::cross_join(estimates, inquiries, suffix = c("", ".inquiry"))
  } else {
    dplyr::left_join(estimates, inquiries, by = shared,
                     suffix = c("", ".inquiry"),
                     relationship = "many-to-many")
  }
  if (nrow(result) > max(nrow(estimates), nrow(inquiries))) {
    matched_on <- if (length(shared) == 0) {
      "no shared column, so every inquiry was attached to every estimate"
    } else {
      paste0("the shared ", paste0("`", shared, "`", collapse = ", "))
    }
    rlang::warn(paste0(
      "Estimates and inquiries were matched on ", matched_on,
      ", which multiplied the rows: ", nrow(result), " rows from ",
      nrow(estimates), " estimates and ", nrow(inquiries), " inquiries.\n",
      "Name the inquiry each estimator targets, ",
      "declare_estimator(..., inquiry = \"", inquiries$inquiry[[1]], "\"), ",
      "so the match is one to one."
    ))
  }
  attr(result, "matched_on") <- shared
  result
}

#' Compute diagnosands on a simulations table
#'
#' @keywords internal
#' @noRd
#' Rewrite diagnosand quosures so a failure yields NA
#'
#' Wraps each diagnosand expression so a missing-column or evaluation error in
#' a single diagnosand yields NA rather than aborting the whole table. Needed
#' when the design has no inquiries (no `estimand` column) but the default
#' diagnosands include bias / coverage. The point estimate and the bootstrap
#' must use the same wrapping, or a design diagnoses at
#' `bootstrap_sims = 0` and errors at any positive value.
#'
#' @keywords internal
#' @noRd
safe_diagnosand_dots <- function(dots) {
  purrr::imap(dots, function(q, nm) {
    expr <- rlang::quo_get_expr(q)
    env  <- rlang::quo_get_env(q)
    # A diagnosand written as a bare constant, `k = 500`, is captured by
    # `enquos()` as a constant quosure, whose environment is the empty one:
    # the expression needs no scope, so rlang gives it none. The wrapper below
    # does need scope, and in the empty environment `tryCatch` cannot be found.
    # The base environment is what a constant's wrapper needs and all it needs.
    if (identical(env, rlang::empty_env())) env <- rlang::base_env()
    rlang::new_quosure(
      rlang::call2(
        "tryCatch",
        rlang::call2("suppressWarnings", expr),
        error = function(e) NA_real_
      ),
      env = env
    )
  })
}

#' Is this a diagnosands set this package can compute with?
#'
#' A diagnosands object built by DeclareDesign carries the same class, the same
#' `step_type` and the same `causal_type` as one built here, so none of those
#' tell the two apart. What distinguishes them is the `dots`: ours are all
#' quosures, and DeclareDesign's begin with the symbol `data`, because its
#' diagnosands are a handler's arguments rather than expressions to evaluate.
#'
#' A design that reaches us carrying the other kind is not an error on the
#' author's part. It is a design someone built with DeclareDesign and
#' `set_diagnosands()`, which is exactly what several DesignLibrary designers
#' do.
#'
#' @keywords internal
#' @noRd
is_own_diagnosands <- function(x) {
  dots <- attr(x, "dots")
  !is.null(dots) && length(dots) > 0 &&
    all(vapply(dots, rlang::is_quosure, logical(1)))
}

#' @keywords internal
#' @noRd
warn_foreign_diagnosands <- function() {
  rlang::warn(paste0(
    "This design carries diagnosands that were not built by ",
    "declare_diagnosands(), so they cannot be read here. ",
    "Diagnosing with the defaults instead.\n",
    "A design built with DeclareDesign and set_diagnosands() looks like this. ",
    "Pass the diagnosands you want directly: ",
    "diagnose_design(design, diagnosands = declare_diagnosands(...))."
  ))
}

compute_diagnosands <- function(simulations_df, diagnosands, group_by_set) {
  dots <- attr(diagnosands, "dots")
  if (is.null(dots)) {
    stop("`diagnosands` must be a declare_diagnosands() object.")
  }
  if (!is_own_diagnosands(diagnosands)) {
    rlang::abort(paste0(
      "`diagnosands` is not a declare_diagnosands() object from this package. ",
      "Its expressions are not quosures, which is what a set of diagnosands ",
      "built by DeclareDesign looks like.\n",
      "Rewrite them with declare_diagnosands()."
    ))
  }
  safe_dots <- safe_diagnosand_dots(dots)
  # A failed draw is dropped here rather than summarised, so `n_sims` counts
  # the draws a diagnosand actually used. Leaving the row in would keep
  # `n_sims` at the number attempted while every default diagnosand, all of
  # which carry na.rm = TRUE, quietly computed on fewer, which hides the
  # shortfall in the one column that should show it.
  if ("error" %in% names(simulations_df)) {
    keep <- is.na(simulations_df$error) | !simulations_df$error
    simulations_df <- simulations_df[keep, , drop = FALSE]
  }
  if (length(group_by_set) == 0) {
    out <- dplyr::summarize(simulations_df, n_sims = dplyr::n(), !!!safe_dots)
    return(tibble::as_tibble(out))
  }
  out <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(n_sims = dplyr::n(), !!!safe_dots, .groups = "drop")
  tibble::as_tibble(out)
}

#' Bootstrap standard errors for diagnosands
#'
#' Resamples the simulations table `B` times and recomputes diagnosands on each
#' replicate to produce bootstrap standard errors.
#'
#' For flat simulations, the unit of resampling is `sim_ID`. For nested
#' simulations (with `model_draw`, `assignment_draw`, etc.), the unit is the
#' outermost draw column, so rows that share a population draw stay together.
#' Resampling at `sim_ID` in the nested case is statistically wrong because
#' rows within the same world are correlated.
#'
#' For nested draws, resamples at the outermost draw level (cluster bootstrap)
#' so that correlated within-world rows are always kept together.
#'
#' Returns a list with `standard_errors` (one row per group, columns named
#' `se(diagnosand)`) and `replicates` (one row per group per replicate), or
#' `NULL` when there is nothing to resample. [compare_diagnoses()] uses the
#' replicates to put a confidence interval on a difference in diagnosands.
#'
#' @keywords internal
#' @noRd
bootstrap_diagnosands <- function(simulations_df, diagnosands, group_by_set,
                                   B, draw_cols = NULL) {
  if (!"sim_ID" %in% names(simulations_df)) return(NULL)
  outer_draw <- if (length(draw_cols) > 0) draw_cols[1] else NULL
  key_col <- outer_draw %||% "sim_ID"
  if (!key_col %in% names(simulations_df)) return(NULL)
  unit_ids <- unique(simulations_df[[key_col]])
  if (length(unit_ids) < 2L) return(NULL)
  diagnosand_names <- names(attr(diagnosands, "dots"))

  dots <- safe_diagnosand_dots(attr(diagnosands, "dots"))

  # Stack all B resampled datasets with a .boot_id tag, then do ONE grouped
  # summarize over the whole thing. This amortises dplyr's per-call NSE
  # overhead across all replicates at once -- ~5x faster than B separate
  # summarize() calls.
  all_boot <- purrr::map(seq_len(B), function(b) {
    drawn  <- sample(unit_ids, length(unit_ids), replace = TRUE)
    lookup <- stats::setNames(data.frame(drawn), key_col)
    r <- dplyr::inner_join(simulations_df, lookup,
                           by = key_col, relationship = "many-to-many")
    r$bootstrap_id <- b
    r
  }) |> dplyr::bind_rows()

  replicate_df <- all_boot |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, "bootstrap_id")))) |>
    dplyr::summarize(!!!dots, .groups = "drop")

  se_groups <- if (length(group_by_set) > 0) group_by_set else character(0)
  out <- replicate_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(se_groups))) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(diagnosand_names),
        ~stats::sd(.x, na.rm = TRUE),
        .names = "se({.col})"
      ),
      .groups = "drop"
    )
  list(standard_errors = tibble::as_tibble(out),
       replicates      = tibble::as_tibble(replicate_df))
}

#' Diagnose a design
#'
#' Simulates the design and computes diagnosands (bias, RMSE, power, coverage,
#' and so on) across simulations. If `bootstrap_sims > 0`, bootstrap standard
#' errors for each diagnosand are also reported.
#'
#' @family simulation and diagnosis
#' @param ... One or more `design` objects.
#' @param sims Number of simulations. Defaults to `NULL`. When `NULL`, designs
#'   with step-level `draws` run in nested mode; otherwise the design runs
#'   `500` flat simulations. When supplied alongside step-level `draws`, the
#'   `draws` are ignored and a warning is emitted.
#' @param bootstrap_sims Number of bootstrap replicates for diagnosand SEs.
#' @param diagnosands A diagnosands `design_step` (e.g., from
#'   [declare_diagnosands()]). Defaults to [default_diagnosands()].
#' @param progress If `TRUE`, display a progress bar for this call by wrapping
#'   it in [progressr::with_progress()]. See [simulate_design()].
#' @return A `diagnosis` object. When the simulation was nested, the result
#'   carries an additional `$variance_decomposition` slot.
#' @export
#' @examples
#' design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' diagnose_design(design, sims = 5, bootstrap_sims = 0)
diagnose_design <- function(..., sims = NULL, bootstrap_sims = 100,
                            diagnosands = NULL, progress = FALSE) {
  if (isTRUE(progress)) {
    return(with_dd_progress(diagnose_design(
      ..., sims = sims, bootstrap_sims = bootstrap_sims,
      diagnosands = diagnosands, progress = FALSE
    )))
  }
  raw <- name_design_dots(...)

  # If first argument is a data frame (pre-computed simulations piped in),
  # skip simulation and go straight to diagnosis. Honours grouping carried by
  # dplyr::group_by() and inherits class-based attrs (draw_cols, parameter_names).
  if (length(raw) >= 1L && is.data.frame(raw[[1L]])) {
    return(diagnose_simulations(
      raw[[1L]],
      bootstrap_sims = bootstrap_sims,
      diagnosands    = diagnosands
    ))
  }

  designs <- flatten_designs(raw)
  if (length(designs) == 0) {
    stop("diagnose_design() requires at least one `design` object.")
  }
  if (is.null(diagnosands)) {
    carried <- Filter(Negate(is.null),
                      purrr::map(designs, function(d) attr(d, "diagnosands")))
    usable <- Filter(is_own_diagnosands, carried)
    if (length(carried) > length(usable)) warn_foreign_diagnosands()
    diagnosands <- if (length(usable) > 0) usable[[1]] else default_diagnosands()
  }
  simulations_df <- rlang::inject(
    simulate_design(!!!designs, sims = sims)
  )
  diagnose_simulations(
    simulations_df,
    diagnosands    = diagnosands,
    bootstrap_sims = bootstrap_sims
  )
}

#' @rdname diagnose_design
#' @export
diagnose_designs <- diagnose_design

#' Diagnose a precomputed simulations table
#'
#' @family simulation and diagnosis
#' @param simulations_df A tibble produced by [simulate_design()].
#' @param diagnosands A diagnosands step (e.g., [default_diagnosands()]).
#' @param bootstrap_sims Number of bootstrap replicates.
#' @return A `diagnosis` object.
#' @export
#' @examples
#' design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' sims <- simulate_design(design, sims = 5)
#' diagnose_simulations(sims, bootstrap_sims = 0)
diagnose_simulations <- function(simulations_df,
                                 diagnosands = NULL,
                                 bootstrap_sims = 100) {
  # Honour group_by() applied upstream -- this is the make_groups replacement
  extra_groups <- character(0)
  if (inherits(simulations_df, "grouped_df")) {
    extra_groups   <- dplyr::group_vars(simulations_df)
    simulations_df <- dplyr::ungroup(simulations_df)
  }
  if (is.null(diagnosands)) diagnosands <- default_diagnosands()
  diagnosis_df <- apply_diagnosand_subset(simulations_df, diagnosands)
  param_cols <- attr(simulations_df, "parameter_names")
  draw_cols <- attr(simulations_df, "draw_cols")
  nested <- isTRUE(attr(simulations_df, "nested")) && length(draw_cols) > 0
  standard_cols <- c("design", param_cols, "inquiry", "estimator", "outcome",
                     "term")
  group_by_set <- c(
    intersect(standard_cols, names(simulations_df)),
    setdiff(extra_groups, standard_cols)
  )
  diagnosands_df <- compute_diagnosands(diagnosis_df, diagnosands,
                                        group_by_set)
  bootstrap_replicates <- NULL
  if (bootstrap_sims > 0) {
    boot <- bootstrap_diagnosands(diagnosis_df, diagnosands, group_by_set,
                                  bootstrap_sims, draw_cols = draw_cols)
    if (!is.null(boot)) {
      bootstrap_replicates <- boot$replicates
      se_df <- boot$standard_errors
      if (length(group_by_set) == 0) {
        diagnosands_df <- dplyr::bind_cols(diagnosands_df, se_df)
      } else {
        diagnosands_df <- dplyr::left_join(diagnosands_df, se_df,
                                           by = group_by_set)
      }
    }
  }
  variance_decomposition <- NULL
  if (nested) {
    variance_decomposition <- compute_variance_decomposition(
      diagnosis_df, draw_cols, group_by_set
    )
  }
  structure(
    list(
      simulations_df         = simulations_df,
      diagnosands_df         = diagnosands_df,
      diagnosand_names       = names(attr(diagnosands, "dots")),
      group_by_set           = group_by_set,
      matched_on             = attr(simulations_df, "matched_on"),
      bootstrap_sims         = bootstrap_sims,
      bootstrap_replicates   = bootstrap_replicates,
      variance_decomposition = variance_decomposition
    ),
    class = "diagnosis"
  )
}

#' Apply a diagnosands step's `subset` to the simulations table
#'
#' @keywords internal
#' @noRd
apply_diagnosand_subset <- function(simulations_df, diagnosands) {
  subset_quo <- attr(diagnosands, "subset_quo")
  if (is.null(subset_quo)) return(simulations_df)
  keep <- rlang::eval_tidy(subset_quo, data = simulations_df)
  if (!is.logical(keep)) {
    rlang::abort("The diagnosands `subset` must evaluate to a logical vector.")
  }
  simulations_df[!is.na(keep) & keep, , drop = FALSE]
}

#' Decompose per-simulation quantity variance by draw level
#'
#' For nested simulations with K levels of draws, uses the law of total
#' variance to attribute the variance of any per-simulation numeric column
#' to each draw level. Level k's contribution is the variance of the
#' conditional mean E[y | L1, ..., Lk], averaged over the outer (L1, ...,
#' L(k-1)) draw. A final residual component captures within-cell variance
#' (stochasticity inside the innermost draw).
#'
#' Column names in the output use the step name extracted from the draw
#' column name by stripping the trailing `_draw` suffix (e.g.
#' `model_draw` -> `var_model`).
#'
#' @param simulations_df A simulations table with `draw_cols` present.
#' @param draw_cols Character vector of draw-tracking columns, ordered
#'   outermost to innermost (e.g. `c("model_draw", "assignment_draw")`).
#' @param group_by_set Character vector of grouping columns (estimator label,
#'   inquiry, redesign parameters, etc.).
#' @param target_cols Character vector of numeric columns in `simulations_df`
#'   to decompose. Defaults to columns that look like per-simulation
#'   quantities (`estimate`, `p.value`, etc.).
#' @return A tibble in long form: one row per (group × target column) with
#'   variance components and their fractions of the total variance.
#' @keywords internal
#' @noRd
compute_variance_decomposition <- function(simulations_df, draw_cols,
                                           group_by_set,
                                           target_cols = NULL) {
  if (length(draw_cols) == 0) return(NULL)

  # Choose columns to decompose if not specified
  if (is.null(target_cols)) {
    candidate <- c("estimate", "p.value", "std.error", "statistic")
    target_cols <- intersect(candidate, names(simulations_df))
  }
  if (length(target_cols) == 0) return(NULL)

  step_names <- sub("_draw$", "", draw_cols)

  join_by <- if (length(group_by_set) > 0) group_by_set else character(0)

  decompose_one <- function(col_name) {
    vals <- simulations_df[[col_name]]
    if (!is.numeric(vals)) return(NULL)

    df <- simulations_df[, c(group_by_set, draw_cols, col_name), drop = FALSE]
    names(df)[ncol(df)] <- ".y"

    # Total variance
    total <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
      dplyr::summarize(n_sims    = dplyr::n(),
                       var_total = stats::var(.y, na.rm = TRUE),
                       .groups   = "drop")

    components <- list(total)

    # Variance attributable to each draw level k
    for (k in seq_along(draw_cols)) {
      outer_k  <- draw_cols[seq_len(k)]
      var_name <- paste0("var_", step_names[k])

      level_means <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, outer_k)))) |>
        dplyr::summarize(.level_mean = mean(.y, na.rm = TRUE), .groups = "drop")

      if (k == 1) {
        comp <- level_means |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
          dplyr::summarize(!!var_name := stats::var(.level_mean, na.rm = TRUE),
                           .groups = "drop")
      } else {
        outer_prev <- draw_cols[seq_len(k - 1L)]
        comp <- level_means |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, outer_prev)))) |>
          dplyr::summarize(.within_var = stats::var(.level_mean, na.rm = TRUE),
                           .groups = "drop") |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
          dplyr::summarize(!!var_name := mean(.within_var, na.rm = TRUE),
                           .groups = "drop")
      }
      components[[length(components) + 1L]] <- comp
    }

    # Residual: within-innermost-cell variance.
    # Cells with 1 row have undefined sample variance; treat as 0 (no
    # within-cell stochasticity when each draw produces a single outcome).
    residual <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, draw_cols)))) |>
      dplyr::summarize(
        .within_var = if (dplyr::n() > 1L) stats::var(.y, na.rm = TRUE) else 0,
        .groups = "drop"
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
      dplyr::summarize(var_residual = mean(.within_var, na.rm = TRUE),
                       .groups = "drop")
    components[[length(components) + 1L]] <- residual

    # Join all components
    result <- purrr::reduce(
      components,
      function(a, b) if (length(join_by) == 0) dplyr::bind_cols(a, b)
                      else dplyr::left_join(a, b, by = join_by)
    )

    # Fractions: normalise over the SUM of components so they always sum to 1.
    # Dividing by var_total can exceed 1 in finite samples because the
    # component estimators (means of conditional variances) are not an exact
    # partition of the sample variance.
    var_cols  <- c(paste0("var_", step_names), "var_residual")
    var_sum   <- Reduce(`+`, lapply(var_cols, function(vc) result[[vc]]))
    for (vc in var_cols) {
      fc <- sub("^var_", "frac_", vc)
      result[[fc]] <- result[[vc]] / var_sum
    }

    result[["quantity"]] <- col_name
    result[["draw_levels"]] <- paste(step_names, collapse = " > ")
    result
  }

  out <- purrr::map(target_cols, decompose_one) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(out) == 0) return(NULL)
  tibble::as_tibble(out)
}
