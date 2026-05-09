test_that("five-step design runs end to end", {
  design <- simple_design(N = 60, ate = 0.5)
  expect_s3_class(design, "design")
  expect_length(design, 5L)

  df <- draw_data(design)
  expect_true(nrow(df) == 60L)
  expect_true(all(c("Y_Z_0", "Y_Z_1", "Z", "Y") %in% names(df)))

  est_df <- draw_estimands(design)
  expect_equal(est_df$inquiry, "ATE")
  expect_equal(est_df$estimand, 0.5)

  est <- draw_estimates(design)
  expect_true("estimate" %in% names(est))
  expect_true("estimand" %in% names(est))
  expect_true("inquiry" %in% names(est))
})

test_that("simulate_design returns one row per sim", {
  design <- simple_design(N = 40)
  sims <- simulate_design(design, sims = 5)
  expect_equal(nrow(sims), 5L)
  expect_true("sim_ID" %in% names(sims))
  expect_true("estimate" %in% names(sims))
  expect_true("estimand" %in% names(sims))
})

test_that("diagnose_design produces standard diagnosands", {
  design <- simple_design(N = 40)
  d <- diagnose_design(design, sims = 10, bootstrap_sims = 0)
  expect_s3_class(d, "diagnosis")
  diag <- get_diagnosands(d)
  for (col in c("bias", "rmse", "power", "coverage")) {
    expect_true(col %in% names(diag))
  }
})

test_that("bootstrap SEs appear when requested", {
  design <- simple_design(N = 30)
  d <- diagnose_design(design, sims = 10, bootstrap_sims = 10)
  diag <- get_diagnosands(d)
  expect_true(any(grepl("^se\\(", names(diag))))
})

test_that("design + NULL returns the design unchanged", {
  d <- declare_model(N = 10, Y = rnorm(N))
  d2 <- d + NULL
  expect_s3_class(d2, "design")
  expect_length(d2, 1L)
})

test_that("run_design rejects non-design input", {
  expect_error(run_design(6), "must be a `design`")
  expect_error(run_design("not a design"), "must be a `design`")
})

test_that("declare_step with handler = fabricate evaluates lazily", {
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatr::fabricate, X2 = X * 2)
  d <- pop + step
  df <- draw_data(d)
  expect_equal(df$X2, df$X * 2)
})
