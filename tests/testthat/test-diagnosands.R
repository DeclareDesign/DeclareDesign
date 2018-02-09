context("Diagnosands")

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


test_that("parallel works.", {


  suppressWarnings(
    diag <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = TRUE, parallel_cores = 1)
  )
  ## diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = TRUE)

  expect_output(print(diag), regexp = "Research design diagnosis")

})

test_that("Diagnosis prints ok", {


  diag <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  ## diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = TRUE)

  expect_output(print(diag), regexp = "Research design diagnosis")

})


test_that("test diagnosands without estimands", {

  my_design2 <- declare_design(my_population,
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes,
                              no_estimand=declare_estimator(Y~Z))

  my_dig <-  declare_diagnosands(mean_est = mean(est), sd_est = sd(est))
  diagnosis <- diagnose_design(my_design2, sims = 2, diagnosands = my_dig, bootstrap = FALSE, parallel = FALSE)

  head(diagnosis$simulations)

  expect_equal(dim(diagnosis$diagnosands), c(1,4))

})


test_that("custom diagnosand function", {



  # default set
  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  mean_custom <- function(x) return(mean(x * 5))

  my_dig <-  declare_diagnosands(mean_x5 = mean_custom(est), mean_true = mean(est))

  rm(mean_custom)
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = FALSE, parallel = FALSE)

  head(diagnosis$simulations)

  diagnosis$diagnosands

  # works with two with bootstrapping
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = TRUE, bootstrap_sims = 2, parallel = FALSE)

  # works with only one diagnosand with bootstrapping (!)
  my_one_dig <-  declare_diagnosands(se_bias = mean(se - sd(estimand)))
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_one_dig, bootstrap = TRUE, parallel = FALSE)

})


test_that("no estimates, no estimators should error", {
  my_population <- declare_population(N = 50)
  my_design <- declare_design(my_population)
  head(draw_data(my_design))

  expect_error(diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE))

})


