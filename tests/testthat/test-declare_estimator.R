test_that("declare_estimator returns a tidy table with estimator label", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
  est <- draw_estimates(design)
  expect_true("estimator" %in% names(est))
  expect_equal(est$estimator, "ols")
  expect_equal(est$term, "Z")
})

test_that("declare_estimator joins to inquiry", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(ATE = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  est <- draw_estimates(design)
  expect_true("estimand" %in% names(est))
  expect_equal(est$inquiry, "ATE")
})

test_that("term filter restricts the rows returned", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), X = rnorm(N),
                          Y = Z + X + rnorm(N)) +
    declare_estimator(Y ~ Z + X, .method = lm, term = "Z", label = "ols")
  est <- draw_estimates(design)
  expect_equal(nrow(est), 1L)
  expect_equal(est$term, "Z")
})

test_that("label_estimator wraps a custom function", {
  my_est <- label_estimator(
    function(data, ...) lm(Y ~ Z, data = data),
    label = "lm", inquiry = "ATE", term = "Z"
  )
  df <- data.frame(Y = rnorm(20), Z = rep(0:1, 10))
  out <- my_est(df)
  expect_equal(out$estimator, "lm")
  expect_equal(out$inquiry, "ATE")
  expect_equal(out$term, "Z")
})

test_that("declare_test does not add an inquiry column", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_test(Y ~ Z, .method = lm, term = "Z", label = "diff")
  est <- draw_estimates(design)
  expect_false("inquiry" %in% names(est))
  expect_equal(est$estimator, "diff")
})
