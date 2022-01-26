context("redesign")

test_that("N not changed", {
  N <- 100
  d <- declare_model(N = N) + NULL
  expect_equal(N, 100)

  expect_length(draw_data(d)$ID, 100)

  others <- c(50, 100, 200, 100)
  d_alt <- redesign(d, N = others)

  for (i in seq_along(others)) {
    expect_length(draw_data(d_alt[[i]])$ID, others[i])
  }

  # N itself should not be changed
  expect_equal(N, 100)
})
