
context("naming steps")

test_that("design steps are named appropriately when blank", {
  pop <- declare_population(n = 25)
  des <- pop + declare_population(N = 50)
  expect_equal(names(des), c("pop", "population"))

  smp <- declare_sampling(n = 25)
  des <- declare_population(N = 50) + smp
  expect_equal(names(des), c("population", "smp"))

  des <- declare_population(N = 50) + declare_potential_outcomes(Y ~ rnorm(N) + 1 * Z) + declare_estimand(mean(Y_Z_1 - Y_Z_0)) + declare_sampling(n = 25) + declare_assignment(m = 10) + declare_estimator(Y ~ Z)
  expect_equal(names(des), c(
    "population", "potential_outcomes", "estimand", "sampling",
    "assignment", "auto_reveal", "estimator"
  ))

  des <- declare_population(N = 50) + declare_potential_outcomes(Y_Z_0 = rnorm(N), Y_Z_1 = Y_Z_0 + 1) + declare_estimand(mean(Y_Z_1 - Y_Z_0)) + declare_sampling(n = 25) + declare_assignment(m = 10) + declare_reveal() + declare_estimator(Y ~ Z)
  expect_equal(names(des), c(
    "population", "potential_outcomes", "estimand", "sampling",
    "assignment", "reveal", "estimator"
  ))
})
