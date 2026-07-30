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

test_that("run_design returns one data frame, not a list of three", {
  design <- simple_design(N = 40)
  one_run <- run_design(design)
  expect_s3_class(one_run, "data.frame")
  expect_equal(nrow(one_run), 1L)
  expect_true(all(c("inquiry", "estimand", "estimate") %in% names(one_run)))
  expect_false("sim_ID" %in% names(one_run))
})

test_that("an estimator with no inquiry = still finds the single inquiry", {
  design <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")

  one_run <- run_design(design)
  expect_equal(one_run$inquiry, "ATE")
  expect_equal(one_run$estimand, 0.5)

  sims <- simulate_design(design, sims = 5)
  expect_true("estimand" %in% names(sims))
  expect_equal(nrow(sims), 5L)

  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_false(is.na(get_diagnosands(d)$bias))
})

test_that("an unlabelled estimator is reported against each inquiry", {
  design <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")
  one_run <- run_design(design)
  expect_equal(one_run$inquiry, c("ATE", "ATT"))
  expect_equal(one_run$estimate, rep(one_run$estimate[1], 2))
})

test_that("estimates and inquiries match on the group columns they share", {
  # Regression test: joining on `inquiry` alone crossed the 3 groups against
  # the 3 groups, and the estimand a group was scored against was arbitrary.
  design <- declare_model(N = 60, g = rep(c("a", "b", "c"), 20),
                          U = rnorm(N), Y = U + as.numeric(g == "b")) +
    declare_inquiry(handler = function(data) {
      data |>
        dplyr::group_by(g) |>
        dplyr::summarize(inquiry = "group_mean", estimand = mean(Y),
                         .groups = "drop")
    }) +
    declare_estimator(handler = function(data) {
      data |>
        dplyr::group_by(g) |>
        dplyr::summarize(term = "mean", estimate = mean(Y), .groups = "drop") |>
        dplyr::mutate(inquiry = "group_mean", estimator = "means")
    })
  one_run <- expect_no_warning(run_design(design))
  expect_equal(nrow(one_run), 3L)
  expect_equal(one_run$estimate, one_run$estimand)
})

test_that("several unlabelled estimators against several inquiries warns", {
  design <- declare_model(N = 40, U = rnorm(N), X = rnorm(N),
                          Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "unadjusted") +
    declare_estimator(Y ~ Z + X, .method = lm, term = "Z", label = "adjusted")
  expect_warning(run_design(design), "multiplied the rows")
})

test_that("declare_step with handler = fabricate evaluates lazily", {
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatrZero::fabricate, X2 = X * 2)
  d <- pop + step
  df <- draw_data(d)
  expect_equal(df$X2, df$X * 2)
})

test_that("declare_step accepts the original fabricatr::fabricate as handler", {
  skip_if_not_installed("fabricatr")
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatr::fabricate, X2 = X * 2)
  df <- draw_data(pop + step)
  expect_equal(df$X2, df$X * 2)
})
