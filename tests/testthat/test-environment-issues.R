context("environment problems")

test_that("send estimand to estimator works", {

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise,
                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

  pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")

  reveal_outcomes <- declare_reveal()

  my_design <- declare_design(my_population(),
                              my_potential_outcomes, pate,
                              my_assignment,
                              reveal_outcomes,
                              pate_estimator)

  rm(list = ls()[-which(ls() %in% "my_design")])
  diag <- diagnose_design(my_design, sims = 2, bootstrap = 3)

  expect_equal(names(diag), c("simulations", "diagnosands", "bootstraps"))
  expect_equal(nrow(diag$simulations), 2)
  expect_equal(nrow(diag$diagnosands), 1)
  expect_equal(nrow(diag$bootstraps), 3)

})
