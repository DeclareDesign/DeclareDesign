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

test_that("expand_design accepts function-valued parameters in zip mode", {
  designer <- function(N = 100, fn = mean) {
    declare_model(N = N, Y = rnorm(N)) +
      declare_inquiry(inq = fn(Y))
  }
  fam <- expand_design(designer, N = c(10, 50),
                       fn = c(mean, median), expand = FALSE)
  expect_length(fam, 2L)
  expect_equal(nrow(draw_data(fam[[1]])), 10L)
  expect_equal(nrow(draw_data(fam[[2]])), 50L)
})

test_that("redesign accepts list-valued parameters", {
  d <- declare_model(N = 30, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = c(.2, .5, .3)))
  fam <- redesign(d, prob_each = list(c(.2, .5, .3), c(0, .5, .5)))
  expect_length(fam, 2L)
  expect_equal(nrow(draw_data(fam[[1]])), 30L)
  expect_equal(nrow(draw_data(fam[[2]])), 30L)
})
