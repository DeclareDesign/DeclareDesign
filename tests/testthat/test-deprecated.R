
context("deprecated")

test_that("get_estimands is deprecated", {
  my_design <- declare_population(N = 100) + NULL
  expect_warning(get_estimands(my_design), "The get_estimands function has been renamed draw_estimands, to make clear that the estimands are draws from a simulation. Please replace your code with draw_estimands.")
})

test_that("get_estimates is changed to draw_estimates", {
  my_design <- declare_population(N = 100) + NULL
  expect_error(get_estimates(my_design), "Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
})
