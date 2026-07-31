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
  prob_each <- c(.2, .5, .3)
  d <- declare_model(N = 300, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = prob_each))
  fam <- redesign(d, prob_each = list(c(.2, .5, .3), c(0, .5, .5)))
  expect_length(fam, 2L)
  expect_equal(nrow(draw_data(fam[[1]])), 300L)
  # The new probabilities really are in force: arm 1 is never assigned.
  expect_equal(unname(table(draw_data(fam[[2]])$Z)["T1"]), 0L)
})

test_that("a bare vector handed to a vector-valued parameter warns", {
  prob_each <- c(.2, .5, .3)
  d <- declare_model(N = 300, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = prob_each))
  expect_warning(redesign(d, prob_each = c(0, .5, .5)),
                 "currently holds 3 values")
  expect_no_warning(redesign(d, prob_each = list(c(0, .5, .5))))

  # sweeping a scalar parameter is the ordinary case and stays quiet
  N <- 30
  scalar <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(scalar, N = c(50, 100)))
})

test_that("a parameter written inline inside a call is not redesignable", {
  # `prob_each` here is the name of an argument to complete_ra(), not a name
  # the design reads out of an environment, so nothing can rebind it. The
  # warning is the whole point: silently returning an unchanged design is how
  # this used to look.
  d <- declare_model(N = 30, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = c(.2, .5, .3)))
  expect_warning(redesign(d, prob_each = list(c(0, .5, .5))),
                 "not found in the design")
})

test_that("redesign warns about a parameter no step responds to", {
  design <- simple_design(N = 30)
  expect_warning(redesign(design, b = 2), "b is not found in the design")
  expect_warning(redesign(design, b = 2, cc = 3), "are not found in the design")
})

test_that("redesign is silent about parameters it does change", {
  N <- 30
  design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(design, N = 50))

  # a literal argument is redesignable too, and must not warn
  literal <- declare_model(N = 30, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(literal, N = 50))
  expect_equal(nrow(draw_data(redesign(literal, N = 50))), 50L)
})

test_that("redesign replaces a function-valued parameter", {
  g <- function(x) mean(x)
  design <- declare_model(N = 40, Y = c(rep(0, 39), 100)) +
    declare_inquiry(inq = g(Y))
  expect_equal(draw_estimands(design)$estimand, 2.5)

  swapped <- redesign(design, g = stats::median)
  expect_s3_class(swapped, "design")
  expect_equal(draw_estimands(swapped)$estimand, 0)

  # varying a function needs a list, one element per design
  fam <- redesign(design, g = list(mean, stats::median))
  expect_length(fam, 2L)
  expect_equal(draw_estimands(fam[[2]])$estimand, 0)
})

test_that("a function-valued parameter reaches the simulations table as its source", {
  g <- function(x) mean(x)
  design <- declare_model(N = 20, Y = rnorm(N)) +
    declare_inquiry(inq = g(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                      inquiry = "inq", label = "ols")
  sims <- simulate_design(redesign(design, g = stats::median), sims = 3)
  expect_true("g" %in% names(sims))
  expect_type(sims$g, "character")
})

test_that("summary lists the parameters and objects the design refers to", {
  N <- 200
  g <- function(x) mean(x)
  design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(inq = g(Y))
  objects <- find_all_objects(design)
  expect_true(all(c("N", "g") %in% objects$name))
  # a package function is not a parameter of the design
  expect_false("rnorm" %in% objects$name)
  # neither is a column an earlier step created
  expect_false("Y" %in% objects$name)
  expect_output(summary(design), "Parameters and objects")
})

test_that("the redesign warning is not silenced by a package of the same name", {
  # Regression test from Macartan's crash course. `env_has_var()` inherited all
  # the way to base, so `redesign(design, n = 200)` on a design with no `n`
  # found `dplyr::n` and stayed quiet. Any short parameter name an attached
  # package exports had the same effect.
  local({
    N <- 100
    design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(Q = 0)
    expect_true(exists("n", envir = as.environment("package:dplyr")))
    expect_warning(redesign(design, n = 200), "n is not found in the design")
    expect_no_warning(redesign(design, N = 200))
  })
})

test_that("a design that reads a package object is still redesignable", {
  skip_if_not_installed("randomizr")
  design <- declare_model(N = 20, Y = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N))
  expect_no_warning(redesign(design, N = 40))
})
