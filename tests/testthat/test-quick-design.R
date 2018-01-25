context("Quick Design")

test_that("quick_design works", {

  two_arm_trial <- function(N){

    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    my_assignment <- declare_assignment(m = N/2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "pate")
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                pate,
                                my_assignment,
                                reveal_outcomes,
                                pate_estimator)
    return(my_design)
  }

  set.seed(1999)
  direct <- draw_data(two_arm_trial(N = 50))

  design <- quick_design(template = two_arm_trial, N = 50)[[1]]
  set.seed(1999)
  qd <- draw_data(design)

  expect_identical(direct, qd)
})



rm(list = ls())

test_that("quick_design works some more", {

  two_arm_trial <- function(N) {
    pop <- declare_population(N = N,
                              Y = rnorm(N),
                              Z = rbinom(N, 1, .5))
    my_estimand <- declare_estimand(mean(Y))
    my_estimator <-
      declare_estimator(Y ~ Z, model = lm_robust, coefficient_name = "Z", estimand = my_estimand)
    my_design <- declare_design(pop, my_estimand, my_estimator)
    return(my_design)
  }

  draw_data(two_arm_trial(N = 5))
  draw_data(two_arm_trial(N = 15))

  a_quick_design <- quick_design(template = two_arm_trial, N = 50)

  execute_design(a_quick_design[[1]])

  diagnose_design(a_quick_design, sims = 2, bootstrap = FALSE, parallel = FALSE)
})


test_that("vary works", {

  two_arm_trial <- function(N, noise_sd){

    my_population <- declare_population(N = N, noise = rnorm(N, sd = noise_sd))
    my_potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    my_assignment <- declare_assignment(m = N/2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "pate")
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                pate,
                                my_assignment,
                                reveal_outcomes,
                                pate_estimator)
    return(my_design)
  }

  design <- quick_design(template = two_arm_trial,
                         N = c(100, 200, 300), noise_sd = 1)

  diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  design <- quick_design(template = two_arm_trial,
                         N = c(100, 200, 300), noise_sd = c(.1, .2, .3))

  diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  design <- quick_design(template = two_arm_trial, expand = FALSE,
                         N = c(100, 200, 300), noise_sd = c(.1, .2, .3))

  diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  expect_error(quick_design(template = two_arm_trial, expand = FALSE,
                         N = c(100, 200, 300), noise_sd = c(.1, .2)))

})

test_that("power curve", {

  two_arm_trial <- function(N){

    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = noise, Y_Z_1 = noise + .25)
    my_assignment <- declare_assignment(m = N/2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "pate")
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                pate,
                                my_assignment,
                                reveal_outcomes,
                                pate_estimator)
    return(my_design)
  }

  design <- quick_design(template = two_arm_trial,
                         N = c(100, 200, 300, 500, 1000))

  diagnosis <- diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)
#
#   library(ggplot2)
#   ggplot(get_diagnosands(diagnosis), aes(x = N, y = power)) +
#     geom_point() +
#     geom_line() +
#     theme_bw()
#

})






