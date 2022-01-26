
context("deprecated")

test_that("get_estimates is changed to draw_estimates", {
  my_design <- declare_model(N = 100) + NULL
  expect_error(get_estimates(my_design), "Please provide a data frame to the data argument. If you would like to get estimates from simulated data, use draw_estimates to draw data and get estimates in one step.")
})


test_that("estimand = is deprecated", {
  
  des <- 
    declare_model(N = 5, Y = rnorm(N)) +
    declare_inquiry(ybar = mean(Y)) +
    declare_estimator(Y ~ 1, model = lm_robust, estimand = "ybar")
  
  expect_warning(draw_estimates(des), "The argument 'estimand = ' is deprecated. Please use 'inquiry = ' instead.")
  
})

test_that("declare_estimand is deprecated", {

expect_warning(declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), label = "The SATE"), "Use 'declare_inquiry' instead.")

expect_warning((
  declare_model(
    N = 10,
    Y_Z_0 = 1:10,
    Y_Z_1 = 3:12
  ) +
    declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))
),
"Use 'declare_inquiry' instead.")

})