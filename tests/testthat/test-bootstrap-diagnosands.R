context("Bootstrap Diagnosands")
test_that("test diagnosands", {
  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = noise,
      Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
    )

  my_assignment <- declare_assignment(m = 25)

  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

  pate_estimator1 <- declare_estimator(Y ~ Z, estimand = pate, label = "test1")
  pate_estimator2 <- declare_estimator(Y ~ Z - 1, estimand = pate, label = "test2")

  reveal <- declare_reveal()

  fixed_data <- my_population()

  my_design <- declare_population(data = fixed_data) +
    my_potential_outcomes +
    pate +
    my_assignment +
    reveal +
    pate_estimator1 +
    pate_estimator2

  # default set

  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap_sims = 2)
  expect_equal(dim(diagnosis$diagnosands_df), c(2,23))

  expect_equal(dim(diagnosis$simulations_df), c(4, 14))
})

