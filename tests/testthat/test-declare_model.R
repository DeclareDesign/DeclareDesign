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

test_that("declare_model accepts a custom handler", {
  myf <- function(N) data.frame(u = rnorm(N))
  step <- declare_model(handler = myf, N = 50)
  out <- step()
  expect_equal(nrow(out), 50L)
  expect_true("u" %in% names(out))
})

test_that("multilevel declare_model nests and draws independently per cluster", {
  # No test touched add_level or nest_level, which is how a recycling bug in
  # the nested path went unnoticed: every cluster received identical residuals.
  skip_if_not_installed("randomizr")
  set.seed(4)
  design <- declare_model(
    villages = add_level(N = 25, u_v = rnorm(N)),
    citizens = add_level(N = 8, e = rnorm(N))
  )
  df <- draw_data(design)
  expect_equal(nrow(df), 200L)
  expect_equal(length(unique(df$villages)), 25L)
  expect_equal(length(unique(df$citizens)), 200L)
  # Cluster-level column is constant within village, unit-level column is not
  expect_equal(length(unique(tapply(df$u_v, df$villages, sd))), 1L)
  expect_true(all(is.na(tapply(df$u_v, df$villages, sd)) |
                    tapply(df$u_v, df$villages, sd) == 0))
  by_village <- split(df$e, df$villages)
  expect_equal(length(unique(by_village)), 25L)
})

test_that("declare_model may declare N alone and add variables downstream", {
  design <- declare_model(N = 80) +
    declare_measurement(Y = rbinom(N, 1, 0.5))
  df <- draw_data(design)
  expect_equal(nrow(df), 80L)
  expect_true("Y" %in% names(df))
})
