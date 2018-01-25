context("Diagnosands")
test_that("parallel works.", {

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise,
                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

  pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")

  my_design <- declare_design(my_population(),
                              my_potential_outcomes, pate,
                              my_assignment,
                              reveal_outcomes(),
                              pate_estimator)

  diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  ## diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = TRUE)

})


test_that("test diagnosands without estimands", {

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise,
                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  estimator <- declare_estimator(Y ~ Z)

  my_design <- declare_design(my_population(),
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes(),
                              estimator)

  my_dig <-  declare_diagnosands(mean_est = mean(est), sd_est = sd(est))
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = FALSE, parallel = FALSE)

  head(diagnosis$simulations)

  diagnosis$diagnosands

})


test_that("custom diagnosand function", {

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise,
                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

  pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")

  my_design <- declare_design(my_population(),
                              my_potential_outcomes, pate,
                              my_assignment,
                              reveal_outcomes(),
                              pate_estimator)

  # default set
  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  mean_custom <- function(x) return(mean(x * 5))

  my_dig <-  declare_diagnosands(mean_x5 = mean_custom(est), mean_true = mean(est))

  rm(mean_custom)
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = FALSE, parallel = FALSE)

  head(diagnosis$simulations)

  diagnosis$diagnosands

})


test_that("no estimates, no estimators should error", {
  my_population <- declare_population(N = 50)
  my_design <- declare_design(my_population)
  head(draw_data(my_design))

  expect_error(diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE))

})


