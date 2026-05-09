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

test_that("term = TRUE returns all model rows including (Intercept)", {
  d <- declare_model(N = 50, X = rnorm(N), Y = rnorm(N) + X) +
    declare_estimator(Y ~ X, .method = lm, term = TRUE, label = "ols")
  est <- draw_estimates(d)
  expect_true(all(c("(Intercept)", "X") %in% est$term))
  expect_equal(nrow(est), 2L)
})

test_that("term and inquiry vectors stay aligned in user-supplied order", {
  d <- declare_model(N = 40, X1 = rnorm(N), X2 = rnorm(N),
                     Y = X1 - X2 + rnorm(N)) +
    declare_inquiry(x1 = 1, x2 = -1, interaction = 0) +
    declare_estimator(Y ~ X1 * X2, .method = lm,
                      term = c("X1:X2", "X1", "X2"),
                      inquiry = c("interaction", "x1", "x2"),
                      label = "ols")
  ret <- run_design(d)
  expect_equal(ret$estimates$term, c("X1:X2", "X1", "X2"))
  expect_equal(ret$estimates$inquiry, c("interaction", "x1", "x2"))
})

test_that("a single estimate row replicates across multiple inquiries", {
  d <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(pate = 1) +
    declare_inquiry(sate = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z",
                      inquiry = c("pate", "sate"), label = "ols")
  e <- draw_estimates(d)
  expect_equal(nrow(e), 2L)
  expect_equal(sort(e$inquiry), c("pate", "sate"))
})

test_that("passing a design_step as inquiry errors with a helpful message", {
  pate <- declare_inquiry(pate = 1)
  expect_error(
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = pate),
    "string, not a step object"
  )
})

test_that("inquiry as a string links estimator to estimand correctly", {
  d <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(pate = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z",
                      inquiry = "pate", label = "ols")
  e <- draw_estimates(d)
  expect_equal(e$inquiry, "pate")
})
