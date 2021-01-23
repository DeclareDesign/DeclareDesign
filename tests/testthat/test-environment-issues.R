context("environment problems")

test_that("send inquiry to estimator works", {
  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = noise,
      Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
    )

  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 25))

  pate <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "pate")

  pate_estimator <- declare_estimator(Y ~ Z, inquiry = pate, label = "test")

  declare_reveal <- declare_reveal()

  my_design <- my_population +
    my_potential_outcomes +
    pate +
    my_assignment +
    declare_reveal +
    pate_estimator

  rm(list = ls()[-which(ls() %in% "my_design")])
  diag <- diagnose_design(my_design, sims = 2, bootstrap_sims = 3)

  expect_equal(names(diag), c("simulations_df", "diagnosands_df", "diagnosand_names", "group_by_set", "parameters_df", "bootstrap_replicates", "bootstrap_sims"))
  expect_equal(nrow(diag$simulations_df), 2)
  expect_equal(nrow(diag$diagnosands_df), 1)
  expect_equal(nrow(diag$bootstrap_replicates), 3)
})
