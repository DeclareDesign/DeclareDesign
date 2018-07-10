
context("deprecated")

test_that("declare_design is deprecated", {
  expect_warning(declare_design(declare_population(N = 5)))
})
