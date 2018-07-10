context("Save-Reload-Redesign")

test_that("Save Reload Redesign works", {
  N <- 100
  d <- declare_population(N = N, foo = rnorm(N)) + declare_estimand(d = mean(foo))
  f <- tempfile()

  suppressWarnings(
    saveRDS(d, f)
  )

  rm(d, N)

  suppressWarnings(
    d <- readRDS(f)
  )

  expect_true(is.data.frame(draw_data(d)))

  d2 <- redesign(d, N = 20)

  expect_true(is.data.frame(draw_data(d2)))
})
