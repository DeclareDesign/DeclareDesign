
context("deprecated")

test_that("get_estimates is changed to draw_estimates", {
  my_design <- declare_population(N = 100) + NULL
  expect_error(get_estimates(my_design), "Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
})
