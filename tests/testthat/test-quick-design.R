library(DeclareDesign)
library(testthat)

test_that("quick_design works", {

  m_arm_trial <- function(N){

    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    my_assignment <- declare_assignment(m = N/2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = pate)
    pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = pate)
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                pate,
                                my_assignment,
                                reveal_outcomes,
                                pate_estimator)
    return(my_design)
  }

  draw_data(m_arm_trial(100))

  design <- quick_design(template = m_arm_trial, N = 100)

  draw_data(design)

  simulate_design(design)

})



rm(list = ls())

test_that("quick_design works some more", {

  m_arm_trial <- function(N) {
    pop <- declare_population(N = N,
                              Y = rnorm(N),
                              Z = rbinom(N, 1, .5))
    my_estimand <- declare_estimand(mean(Y))
    my_estimator <-
      declare_estimator(Y ~ Z, estimator_function = lm_robust_se, coefficient_name = "Z", estimand = my_estimand)
    my_design <- declare_design(pop, my_estimand, my_estimator)
    return(my_design)
  }

m_arm_trial(N = 5)$data_function()
m_arm_trial(N = 15)$data_function()


a_quick_design <- quick_design(template = m_arm_trial, N = 100)

a_quick_design$design_function()

diagnose_design(a_quick_design, sims = 20)
})
