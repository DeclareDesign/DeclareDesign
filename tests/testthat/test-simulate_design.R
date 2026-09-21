d1 <- function() declare_model(N = 6, Y = rnorm(N)) + declare_inquiry(m = mean(Y))
d2 <- function() declare_model(N = 8, Y = rnorm(N)) + declare_inquiry(m = mean(Y))

test_that("simulate_design needs a design", {
  expect_error(simulate_design(sims = 2), "requires at least one `design`")
})

test_that("a bare step simulates as a one-step design", {
  sims <- simulate_design(declare_inquiry(m = 1), sims = 2)
  expect_equal(unique(sims$design), "design_1")
  expect_equal(sims$estimand, c(1, 1))
})

test_that("a list of designs is labelled by the name it was supplied under", {
  # One design in a named list takes the name itself, since there is nothing
  # to tell apart; several take the name as a prefix.
  one <- simulate_design(a = list(d1()), sims = 2)
  expect_equal(unique(one$design), "a")
  two <- simulate_design(a = list(d1(), d2()), sims = 2)
  expect_equal(unique(two$design), c("a_design_1", "a_design_2"))
})

test_that("two lists carrying the same inner name are held apart", {
  sims <- simulate_design(list(x = d1(), y = d2()), list(x = d2()), sims = 2)
  expect_equal(unique(sims$design), c("x", "y", "x.1"))
})

test_that("`add_grouping_variables` names its replacement rather than being ignored", {
  # It fell through the design filter and was silently dropped, so a grouped
  # simulation came back ungrouped while the migration vignette said it
  # errored.
  expect_error(
    simulate_design(d1(), sims = 2, add_grouping_variables = "N"),
    "group the simulations with `group_by\\(\\)`"
  )
})
