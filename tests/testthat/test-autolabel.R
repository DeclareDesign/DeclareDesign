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
