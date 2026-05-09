#' Simulate one or more designs
#'
#' Runs each design `sims` times, collecting estimands and estimates into a
#' single long tibble suitable for diagnosis. When more than one design is
#' supplied, a `design` column distinguishes them.
#'
#' @param ... One or more `design` objects.
#' @param sims Number of simulations per design.
#' @return A tibble of stacked simulation results.
#' @export
#' @examples
#' design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
#'   declare_inquiry(ATE = 1) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' simulate_design(design, sims = 3)
simulate_design <- function(..., sims = 500) {
  designs <- rlang::dots_list(..., .named = TRUE)
  designs <- lapply(designs, function(d) {
    if (inherits(d, "design_step")) {
      construct_design(wrap_step(d))
    } else d
  })
  designs <- Filter(function(d) inherits(d, "design"), designs)
  if (length(designs) == 0) {
    stop("simulate_design() requires at least one `design` object.")
  }
  multi <- length(designs) > 1L
  per_design <- purrr::imap(designs, function(design, design_label) {
    one_design_sims(design, sims = sims, design_label = design_label,
                    multi = multi)
  })
  dplyr::bind_rows(per_design)
}

#' @rdname simulate_design
#' @export
simulate_designs <- simulate_design

#' Simulate a single design
#'
#' @keywords internal
#' @noRd
one_design_sims <- function(design, sims, design_label = "design",
                            multi = FALSE) {
  results <- purrr::map(seq_len(sims), function(i) {
    r <- run_design(design)
    list(inquiries = r$inquiries, estimates = r$estimates)
  })
  inquiries_df <- dplyr::bind_rows(
    purrr::map(results, "inquiries"),
    .id = "sim_ID"
  )
  estimates_df <- dplyr::bind_rows(
    purrr::map(results, "estimates"),
    .id = "sim_ID"
  )
  if (nrow(inquiries_df) > 0) {
    inquiries_df$sim_ID <- as.integer(inquiries_df$sim_ID)
  }
  if (nrow(estimates_df) > 0) {
    estimates_df$sim_ID <- as.integer(estimates_df$sim_ID)
  }
  out <- merge_estimates_inquiries(estimates_df, inquiries_df)
  if (multi && nrow(out) > 0) {
    out$design <- design_label
    out <- dplyr::relocate(out, "design")
  }
  out
}
