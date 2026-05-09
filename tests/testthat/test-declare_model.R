test_that("declare_model on first step uses NULL data", {
  step <- declare_model(N = 25, X = rnorm(N))
  df <- step(NULL)
  expect_equal(nrow(df), 25L)
  expect_true("X" %in% names(df))
})

test_that("declare_measurement adds columns to existing data", {
  m <- declare_model(N = 20, U = rnorm(N))
  meas <- declare_measurement(Y = U + 1)
  d1 <- m(NULL)
  d2 <- meas(d1)
  expect_equal(nrow(d2), 20L)
  expect_true("Y" %in% names(d2))
})

test_that("declare_sampling filters by S column by default", {
  step <- declare_sampling(S = rep(c(1, 0), length.out = 10))
  df <- data.frame(ID = seq_len(10))
  out <- step(df)
  expect_true(all(out$S == 1))
})

test_that("declare_sampling honors filter expression", {
  step <- declare_sampling(X = seq_len(10), filter = X > 5)
  df <- data.frame(ID = seq_len(10))
  out <- step(df)
  expect_true(all(out$X > 5))
})

test_that("design step labels are inferred", {
  d <- declare_model(N = 5, Y = rnorm(N), label = "popgen")
  expect_equal(attr(d, "label"), "popgen")
})

test_that("step type and causal type attributes are set", {
  m <- declare_model(N = 5, Y = rnorm(N))
  expect_equal(attr(m, "step_type"), "model")
  expect_equal(attr(m, "causal_type"), "dgp")

  i <- declare_inquiry(mu = mean(Y))
  expect_equal(attr(i, "step_type"), "inquiry")
  expect_equal(attr(i, "causal_type"), "inquiry")
})
