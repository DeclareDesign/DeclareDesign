context("Reshape Diagnosis")

test_that("reshape works", {
  
  N <- 500
  
  my_population <- declare_population(N = N, noise = rnorm(N))
  
  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  
  my_sampling <- declare_sampling(n = 250)
  
  my_assignment <- declare_assignment(m = 25)
  
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  
  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
  
  my_reveal <- declare_reveal()
  
  design <- my_population +
    my_potential_outcomes +
    my_sampling +
    my_estimand +
    tidy_step(dplyr::mutate(q = 5)) +
    my_assignment +
    my_reveal +
    my_estimator
  
  dx <- diagnose_design(design, sims = 10, bootstrap_sims = 5)
  #debugonce(reshape_diagnosis)
  reshape_diagnosis(dx)
  expect_error(reshape_diagnosis(dx, select = "mean_estimand"))
  reshape_diagnosis(dx, select = "Mean Estimand")
})
