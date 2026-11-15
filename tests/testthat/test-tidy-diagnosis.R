test_that("tidy() works when a design parameter is named alpha", {
  alpha_designer <- function(alpha = 0.5) {
    declare_model(N = 20, Y = rnorm(N, mean = alpha)) +
      declare_inquiry(mu = alpha) +
      declare_estimator(Y ~ 1, .method = lm, inquiry = "mu", label = "ols")
  }
  designs <- redesign(alpha_designer(), alpha = c(0.25, 0.5, 0.75))
  set.seed(343)
  d <- diagnose_design(designs, sims = 3, bootstrap_sims = 5)
  expect_true("alpha" %in% names(d$diagnosands_df))
  out <- tidy(d)
  expect_true(all(c("alpha", "conf.low", "conf.high") %in% names(out)))
  expect_equal(nrow(out), nrow(tidyr::pivot_longer(
    d$diagnosands_df, dplyr::all_of(d$diagnosand_names))))
  expect_false(anyNA(out$conf.low))
  expect_true(all(out$conf.low <= out$conf.high))
})
