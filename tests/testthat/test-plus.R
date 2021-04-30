context("plus operator")

test_that("plus works", {
  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  my_sampling <- declare_sampling(S = complete_rs(N, n = 250))
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
  my_reveal <- declare_reveal()

  design <- my_population + my_potential_outcomes + my_sampling + my_inquiry + my_assignment + my_reveal + my_estimator
  expect_length(design, 7)
})




test_that("more plus", {
  U <- declare_population(N = 10, noise = rnorm(N))
  Y <- declare_potential_outcomes(Y ~ Z + noise)
  Z <- declare_assignment(Z = complete_ra(N, prob = 0.5))
  R <- declare_reveal()

  # doesn't
  # expect_warning(des <- U + Y + Z + R)

  des <- U + Y + Z + R
  expect_null(attr(des[[4]], "label"))
})
