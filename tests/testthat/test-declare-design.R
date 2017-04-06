test_that("test the full declare design setup", {

  library(dplyr)
  library(DDestimate)

  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_assignment <- declare_assignment(m = 25)

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

  my_mutate_wrapper <- function(data){
    mutate(data, var = 5)
  }

  design <- declare_design(my_population(),
                           my_potential_outcomes,
                           my_estimand,
                           ##declare_step(mutate(var = 5)),
                           ##my_mutate_wrapper
                           my_sampling,
                           my_assignment,
                           reveal_outcomes,
                           my_estimator)

  design$data_function()

  design$design_function()


  diagnose_design(design = design, diagnosands = declare_diagnosands(superpower = mean(p < .555)))

  rm(list = ls())

  m_arm_trial <- function(N){
    pop <- declare_population(N = N, Y = rnorm(N), Z = rbinom(N, 1, .5))
    pate_estimator <- declare_estimator(Y ~ Z, estimator_function = lm_robust_se)
    my_design <- declare_design(pop(), pate_estimator)
    return(my_design)
  }

  m_arm_trial(N = 5)$data_function()


  quick_design(template = m_arm_trial, N = 5)$design_function()


})
