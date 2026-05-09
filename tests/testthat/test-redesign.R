test_that("redesign returns a single design when one combination is supplied", {
  design <- simple_design(N = 30)
  d2 <- redesign(design, N = 100)
  expect_s3_class(d2, "design")
  df <- draw_data(d2)
  expect_equal(nrow(df), 100L)
})

test_that("redesign expands the parameter grid by default", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  expect_length(fam, 2L)
  ns <- vapply(fam, function(d) nrow(draw_data(d)), integer(1))
  expect_equal(unname(sort(ns)), c(20L, 40L))
})

test_that("redesign with expand = FALSE zips parameters", {
  designer <- function(N, ate) {
    declare_model(N = N, Y = rnorm(N) + ate) +
      declare_inquiry(mu = ate)
  }
  design <- designer(N = 50, ate = 0.5)
  fam <- redesign(design, N = c(25, 75), ate = c(0.1, 0.2), expand = FALSE)
  expect_length(fam, 2L)
})

test_that("expand_design builds a list from a designer", {
  designer <- function(N) declare_model(N = N, Y = rnorm(N))
  fam <- expand_design(designer, N = c(10, 20))
  expect_length(fam, 2L)
})
