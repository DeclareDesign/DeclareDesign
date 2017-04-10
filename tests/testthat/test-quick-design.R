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

    simulate_design(my_design)

    return(my_design)
  }

  design <- quick_design(template = m_arm_trial, N = 1000)

  draw_data(design)

  simulate_design(design)

})
