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
  if (length(group_by_set) == 0) {
    out <- dplyr::summarize(simulations_df, !!!dots)
    return(tibble::as_tibble(out))
  }
  out <- simulations_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_set))) |>
    dplyr::summarize(!!!dots, .groups = "drop")
  tibble::as_tibble(out)
}

#' Bootstrap standard errors for diagnosands
#'
#' @keywords internal
#' @noRd
bootstrap_diagnosands <- function(simulations_df, diagnosands, group_by_set, B) {
  if (!"sim_ID" %in% names(simulations_df)) return(NULL)
  sim_ids <- unique(simulations_df$sim_ID)
  if (length(sim_ids) < 2L) return(NULL)
  diagnosand_names <- names(attr(diagnosands, "dots"))
  replicates <- purrr::map(seq_len(B), function(i) {
    drawn <- sample(sim_ids, length(sim_ids), replace = TRUE)
    drawn_df <- tibble::tibble(sim_ID = drawn,
                               .boot_id = seq_along(drawn))
    resampled <- dplyr::inner_join(simulations_df, drawn_df,
                                   by = "sim_ID",
                                   relationship = "many-to-many")
    compute_diagnosands(resampled, diagnosands, group_by_set)
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
#' @param sims Number of simulations.
#' @param bootstrap_sims Number of bootstrap replicates for diagnosand SEs.
#' @param diagnosands A diagnosands `design_step` (e.g., from
#'   [declare_diagnosands()]). Defaults to [default_diagnosands()].
#' @return A `diagnosis` object.
#' @export
#' @examples
#' design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' diagnose_design(design, sims = 5, bootstrap_sims = 0)
diagnose_design <- function(..., sims = 500, bootstrap_sims = 100,
                            diagnosands = NULL) {
  designs <- rlang::dots_list(..., .named = TRUE)
  designs <- lapply(designs, function(d) {
    if (inherits(d, "design_step")) {
      construct_design(wrap_step(d))
    } else d
  })
  designs <- Filter(function(d) inherits(d, "design"), designs)
  if (length(designs) == 0) {
    stop("diagnose_design() requires at least one `design` object.")
  }
  if (is.null(diagnosands)) {
    user_diags <- purrr::map(designs, function(d) attr(d, "diagnosands"))
    user_diags <- Filter(Negate(is.null), user_diags)
    diagnosands <- if (length(user_diags) > 0) user_diags[[1]] else default_diagnosands()
  }
  simulations_df <- simulate_design(!!!designs, sims = sims)
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
                                 diagnosands = default_diagnosands(),
                                 bootstrap_sims = 100) {
  group_by_set <- intersect(
    c("design", "inquiry", "estimator", "outcome", "term"),
    names(simulations_df)
  )
  diagnosands_df <- compute_diagnosands(simulations_df, diagnosands,
                                        group_by_set)
  if (bootstrap_sims > 0) {
    se_df <- bootstrap_diagnosands(simulations_df, diagnosands, group_by_set,
                                   bootstrap_sims)
    if (!is.null(se_df)) {
      if (length(group_by_set) == 0) {
        diagnosands_df <- dplyr::bind_cols(diagnosands_df, se_df)
      } else {
        diagnosands_df <- dplyr::left_join(diagnosands_df, se_df,
                                           by = group_by_set)
      }
    }
  }
  structure(
    list(
      simulations_df  = simulations_df,
      diagnosands_df  = diagnosands_df,
      diagnosand_names = names(attr(diagnosands, "dots")),
      group_by_set    = group_by_set,
      bootstrap_sims  = bootstrap_sims
    ),
    class = "diagnosis"
  )
}
