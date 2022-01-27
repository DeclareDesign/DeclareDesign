

context("get_ functions")

population <- declare_model(N = 100, u = rnorm(N))
potential_outcomes <- declare_model(Y_Z_0 = 0,
                                                 Y_Z_1 = 1 + u)
inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
sampling <- declare_sampling(S = complete_rs(N, n = 75))
assignment <- declare_assignment(Z = complete_ra(N, m = 50))
measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
estimator <- declare_estimator(Y ~ Z, inquiry = inquiry)
design <-
  population + potential_outcomes + inquiry + sampling + assignment + measurement + estimator

dat <- draw_data(design)
dat$Z <- NULL

test_that("error when send list of designs to draw_data", {
  
  expect_error(draw_data(list(design, design)), "Please send a single design object to the design argument, typically created using the \\+ operator.")
  
})
