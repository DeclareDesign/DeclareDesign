test_that("two default estimators get formula-based autolabels", {
  design <- suppressMessages(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm) +
    declare_estimator(Y ~ Z + 1, .method = lm)
  )
  lbls <- vapply(unclass(design), function(s) attr(s, "label"), character(1))
  expect_true(all(c("Y~Z", "Y~Z+1") %in% lbls))
})

test_that("same formula different method gets method appended", {
  design <- suppressMessages(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm) +
    declare_estimator(Y ~ Z, .method = estimatr::lm_robust)
  )
  est_steps <- unclass(design)[
    vapply(unclass(design), function(s) identical(attr(s, "step_type"), "estimator"), logical(1))
  ]
  lbls <- vapply(est_steps, function(s) attr(s, "label"), character(1))
  expect_equal(length(unique(lbls)), 2L)
  expect_true(all(grepl("Y~Z", lbls)))
})

test_that("truly duplicate estimators get .a .b suffix", {
  design <- suppressMessages(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm, label = "ols") +
    declare_estimator(Y ~ Z, .method = lm, label = "ols")
  )
  est_steps <- unclass(design)[
    vapply(unclass(design), function(s) identical(attr(s, "step_type"), "estimator"), logical(1))
  ]
  lbls <- vapply(est_steps, function(s) attr(s, "label"), character(1))
  expect_true(all(c("ols.a", "ols.b") %in% lbls))
})

test_that("estimator column in simulations uses autolabel not original label", {
  design <- suppressMessages(
    declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ Z, .method = lm) +
    declare_estimator(Y ~ Z + 0, .method = lm)
  )
  sim <- suppressMessages(simulate_design(design, sims = 2))
  est_labels <- unique(sim$estimator)
  expect_equal(length(est_labels), 2L)
  expect_false(any(duplicated(est_labels)))
})

test_that("autolabel emits an inform message on relabel", {
  expect_message(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
      declare_estimator(Y ~ Z, .method = lm) +
      declare_estimator(Y ~ Z + 1, .method = lm),
    "auto-labeled"
  )
})

test_that("single estimator is not autolabeled", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm, label = "my_est")
  est_steps <- unclass(design)[
    vapply(unclass(design), function(s) identical(attr(s, "step_type"), "estimator"), logical(1))
  ]
  expect_equal(attr(est_steps[[1]], "label"), "my_est")
})

# diagnose_design as unified entry point
test_that("simulate_design |> diagnose_design() works (df piped in)", {
  design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu")
  diag <- design |> simulate_design(sims = 5) |>
    diagnose_design(bootstrap_sims = 0)
  expect_s3_class(diag, "diagnosis")
})

test_that("group_by() upstream of diagnose_simulations adds groups", {
  design <- declare_model(N = 50, Y = rnorm(N), Z = rep(0:1, 25)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu")
  diag <- design |>
    simulate_design(sims = 10) |>
    dplyr::mutate(big = estimate > 0) |>
    dplyr::group_by(big) |>
    diagnose_simulations(bootstrap_sims = 0)
  expect_true("big" %in% names(diag$diagnosands_df))
  expect_equal(nrow(diag$diagnosands_df), 2L)
})

test_that("group_by |> diagnose_design() works end-to-end", {
  design <- declare_model(N = 50, Y = rnorm(N), Z = rep(0:1, 25)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu")
  diag <- design |>
    simulate_design(sims = 10) |>
    dplyr::mutate(sig = p.value < 0.5) |>
    dplyr::group_by(sig) |>
    diagnose_design(bootstrap_sims = 0)
  expect_s3_class(diag, "diagnosis")
  expect_true("sig" %in% names(diag$diagnosands_df))
})

test_that("declare_population warns and works", {
  expect_warning(
    m <- declare_population(N = 10, Y = rnorm(N)),
    "deprecated"
  )
  expect_s3_class(m, "design_step")
})

test_that("compare_designs errors informatively", {
  expect_error(compare_designs(), "not implemented")
  expect_error(compare_design_code(), "not implemented")
  expect_error(compare_design_data(), "not implemented")
  expect_error(compare_design_estimates(), "not implemented")
  expect_error(compare_design_inquiries(), "not implemented")
  expect_error(compare_design_summaries(), "not implemented")
  expect_error(print_code(), "not implemented")
})

test_that("model_handler / tidy_estimator warn and forward to label_estimator", {
  expect_warning(
    fn <- model_handler(function(data, ...) lm(Y ~ Z, data = data),
                        label = "lm", inquiry = "ATE", term = "Z"),
    "deprecated"
  )
  expect_true(is.function(fn))
  expect_warning(
    fn2 <- tidy_estimator(function(data, ...) lm(Y ~ Z, data = data),
                          label = "lm", inquiry = "ATE", term = "Z"),
    "deprecated"
  )
  expect_true(is.function(fn2))
})
