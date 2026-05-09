#' Join estimates to their inquiries
#'
#' Used in [draw_estimates()] and [simulate_design()] to attach estimands to
#' estimator output via the `inquiry` label.
#'
#' @param estimates A tibble of estimator output.
#' @param inquiries A tibble of inquiry output.
#' @return A tibble.
#' @export
#' @keywords internal
merge_estimates_inquiries <- function(estimates, inquiries) {
  if (nrow(estimates) == 0 && nrow(inquiries) == 0) return(tibble::tibble())
  if (nrow(estimates) == 0) return(tibble::as_tibble(inquiries))
  if (nrow(inquiries) == 0) return(tibble::as_tibble(estimates))
  if (!"inquiry" %in% names(estimates)) {
    return(tibble::as_tibble(estimates))
  }
  join_cols <- intersect(
    intersect(c("design", "sim_ID", "inquiry"), names(estimates)),
    names(inquiries)
  )
  if (length(join_cols) == 0) return(tibble::as_tibble(estimates))
  result <- dplyr::left_join(
    tibble::as_tibble(estimates),
    tibble::as_tibble(inquiries),
    by = join_cols,
    suffix = c("", ".inquiry")
  )
  result
}

#' Compute diagnosands on a simulations table
#'
#' @keywords internal
#' @noRd
compute_diagnosands <- function(simulations_df, diagnosands, group_by_set) {
  dots <- attr(diagnosands, "dots")
  if (is.null(dots)) {
    stop("`diagnosands` must be a declare_diagnosands() object.")
  }
  # Wrap each diagnosand expression so a missing-column or evaluation error
  # in a single diagnosand yields NA rather than aborting the whole table.
  # Useful when the design has no inquiries (no `estimand` column) but the
  # default diagnosands include bias / coverage.
  safe_dots <- purrr::imap(dots, function(q, nm) {
    expr <- rlang::quo_get_expr(q)
    env  <- rlang::quo_get_env(q)
    rlang::new_quosure(
      rlang::call2(
        "tryCatch",
        rlang::call2("suppressWarnings", expr),
        error   = rlang::call2("function", as.pairlist(alist(e = )),
                                quote(NA_real_))
      ),
      env = env
    )
  })
  if (length(group_by_set) == 0) {
    out <- dplyr::summarize(simulations_df, !!!safe_dots)
    return(tibble::as_tibble(out))
  }
  out <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(!!!safe_dots, .groups = "drop")
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

  resample_once <- function() {
    drawn  <- sample(unit_ids, length(unit_ids), replace = TRUE)
    lookup <- stats::setNames(data.frame(drawn), key_col)
    # many-to-many join: units drawn k times appear k times in the output,
    # giving them k-fold weight in subsequent summarize() calls -- correct
    # bootstrap behaviour without any sim_ID reindexing.
    dplyr::inner_join(simulations_df, lookup,
                      by = key_col, relationship = "many-to-many")
  }

  replicates <- purrr::map(seq_len(B), function(b) {
    compute_diagnosands(resample_once(), diagnosands, group_by_set)
  })
  replicate_df <- dplyr::bind_rows(replicates)
  if (length(group_by_set) == 0) {
    se_row <- dplyr::summarize(
      replicate_df,
      dplyr::across(
        dplyr::all_of(diagnosand_names),
        ~stats::sd(.x, na.rm = TRUE),
        .names = "se({.col})"
      )
    )
    return(tibble::as_tibble(se_row))
  }
  out <- replicate_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(diagnosand_names),
        ~stats::sd(.x, na.rm = TRUE),
        .names = "se({.col})"
      ),
      .groups = "drop"
    )
  tibble::as_tibble(out)
}

#' Diagnose a design
#'
#' Simulates the design and computes diagnosands (bias, RMSE, power, coverage,
#' and so on) across simulations. If `bootstrap_sims > 0`, bootstrap standard
#' errors for each diagnosand are also reported.
#'
#' @param ... One or more `design` objects.
#' @param sims Number of simulations. Defaults to `NULL`. When `NULL`, designs
#'   with step-level `draws` run in nested mode; otherwise the design runs
#'   `500` flat simulations. When supplied alongside step-level `draws`, the
#'   `draws` are ignored and a warning is emitted.
#' @param bootstrap_sims Number of bootstrap replicates for diagnosand SEs.
#' @param diagnosands A diagnosands `design_step` (e.g., from
#'   [declare_diagnosands()]). Defaults to [default_diagnosands()].
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
                            diagnosands = NULL) {
  raw <- rlang::dots_list(..., .named = FALSE)

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

  raw_named <- rlang::dots_list(..., .named = TRUE)
  designs <- flatten_designs(raw_named)
  if (length(designs) == 0) {
    stop("diagnose_design() requires at least one `design` object.")
  }
  if (is.null(diagnosands)) {
    user_diags <- purrr::map(designs, function(d) attr(d, "diagnosands"))
    user_diags <- Filter(Negate(is.null), user_diags)
    diagnosands <- if (length(user_diags) > 0) user_diags[[1]] else default_diagnosands()
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
  param_cols <- attr(simulations_df, "parameter_names")
  draw_cols <- attr(simulations_df, "draw_cols")
  nested <- isTRUE(attr(simulations_df, "nested")) && length(draw_cols) > 0
  standard_cols <- c("design", param_cols, "inquiry", "estimator", "outcome",
                     "term")
  group_by_set <- c(
    intersect(standard_cols, names(simulations_df)),
    setdiff(extra_groups, standard_cols)
  )
  diagnosands_df <- compute_diagnosands(simulations_df, diagnosands,
                                        group_by_set)
  if (bootstrap_sims > 0) {
    se_df <- bootstrap_diagnosands(simulations_df, diagnosands, group_by_set,
                                   bootstrap_sims, draw_cols = draw_cols)
    if (!is.null(se_df)) {
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
      simulations_df, draw_cols, group_by_set
    )
  }
  structure(
    list(
      simulations_df         = simulations_df,
      diagnosands_df         = diagnosands_df,
      diagnosand_names       = names(attr(diagnosands, "dots")),
      group_by_set           = group_by_set,
      bootstrap_sims         = bootstrap_sims,
      variance_decomposition = variance_decomposition
    ),
    class = "diagnosis"
  )
}

#' Decompose estimate variance by simulation draw level
#'
#' For nested simulations, splits the total variance of estimates into the
#' part attributable to design-level fluctuation (within a single fixed world)
#' and the part attributable to world-level fluctuation (variance of the
#' average estimate across worlds). The split uses the law of total variance
#' with the outermost step that has `draws > 1` as the world identifier.
#'
#' @param simulations_df A simulations table with `draw_cols` attached.
#' @param draw_cols Character vector of draw-tracking columns, ordered
#'   outermost to innermost.
#' @param group_by_set Character vector of grouping columns.
#' @return A tibble with one row per group containing variance components and
#'   their fractions of the total.
#' @keywords internal
#' @noRd
compute_variance_decomposition <- function(simulations_df, draw_cols,
                                           group_by_set) {
  if (!"estimate" %in% names(simulations_df)) return(NULL)
  if (length(draw_cols) == 0) return(NULL)
  outer_draw <- draw_cols[1]
  total <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(
      n_sims    = dplyr::n(),
      var_total = stats::var(estimate, na.rm = TRUE),
      .groups   = "drop"
    )
  design_var <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, outer_draw)))) |>
    dplyr::summarize(within_var = stats::var(estimate, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(var_design = mean(within_var, na.rm = TRUE),
                     .groups = "drop")
  world_var <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_set, outer_draw)))) |>
    dplyr::summarize(world_mean = mean(estimate, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(var_world = stats::var(world_mean, na.rm = TRUE),
                     .groups = "drop")
  if (length(group_by_set) == 0) {
    result <- dplyr::bind_cols(total, design_var, world_var)
  } else {
    result <- total |>
      dplyr::left_join(design_var, by = group_by_set) |>
      dplyr::left_join(world_var,  by = group_by_set)
  }
  result <- dplyr::mutate(
    result,
    frac_design = var_design / var_total,
    frac_world  = var_world  / var_total,
    sd_design   = sqrt(var_design),
    sd_world    = sqrt(var_world),
    sd_total    = sqrt(var_total),
    draw_levels = paste(draw_cols, collapse = " > ")
  )
  tibble::as_tibble(result)
}
