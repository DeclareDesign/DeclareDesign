# The label an estimator gets when none is given, and how two that collide
# are told apart: `autolabel_estimators()` in R/aaa_classes.R.

test_that("two default estimators get formula-based autolabels", {
  design <- suppressMessages(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm) +
    declare_estimator(Y ~ Z + 1, .method = lm)
  )
  lbls <- vapply(unclass(design), function(s) attr(s, "label"), character(1))
  expect_true(all(c("Y~Z", "Y~Z+1") %in% lbls))
})

test_that("the same formula under a different method has the method appended", {
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

test_that("identical estimators are suffixed .a and .b", {
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

test_that("the estimator column in simulations carries the autolabel, not the label given", {
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

test_that("relabelling an estimator emits a message", {
  expect_message(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
      declare_estimator(Y ~ Z, .method = lm) +
      declare_estimator(Y ~ Z + 1, .method = lm),
    "auto-labeled"
  )
})

test_that("a single estimator is not autolabelled", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(Y ~ Z, .method = lm, label = "my_est")
  est_steps <- unclass(design)[
    vapply(unclass(design), function(s) identical(attr(s, "step_type"), "estimator"), logical(1))
  ]
  expect_equal(attr(est_steps[[1]], "label"), "my_est")
})

test_that("two unlabelled identical estimators are suffixed on the inferred label", {
  # The suffix goes on the label the user set, when they set one and set the
  # same one twice (`ols.a`, `ols.b` above). With neither labelled, the
  # original is the default `estimator` and carries no information, so the
  # suffix goes on the inferred formula label instead.
  design <- suppressMessages(
    declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
      declare_estimator(Y ~ Z, .method = lm, term = "Z") +
      declare_estimator(Y ~ Z, .method = lm, term = "Z")
  )
  lbls <- vapply(unclass(design), function(s) attr(s, "label"), character(1))
  expect_true(all(c("Y~Z.a", "Y~Z.b") %in% lbls))
})
