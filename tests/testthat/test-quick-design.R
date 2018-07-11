context("Quick Design")

test_that("expand_design works", {
  two_arm_trial <- function(N) {
    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    my_assignment <- declare_assignment(m = N / 2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <-
      declare_estimator(Y ~ Z, estimand = pate, label = "pate_hat")
    reveal_outcomes <- declare_reveal()
    my_design <- my_population +
      my_potential_outcomes +
      pate +
      my_assignment +
      reveal_outcomes +
      pate_estimator
    return(my_design)
  }

  set.seed(1999)
  direct <- draw_data(two_arm_trial(N = 50))

  design <- expand_design(designer = two_arm_trial, N = 50)
  set.seed(1999)
  qd <- draw_data(design)

  expect_identical(direct, qd)
})

rm(list = ls())

test_that("expand_design works some more", {
  two_arm_trial <- function(N) {
    pop <- declare_population(
      N = N,
      Y = rnorm(N),
      Z = rbinom(N, 1, .5)
    )
    my_estimand <- declare_estimand(mean(Y))
    my_estimator <-
      declare_estimator(Y ~ Z,
        model = lm_robust,
        term = "Z",
        estimand = my_estimand
      )
    my_design <- pop + my_estimand + my_estimator
    return(my_design)
  }

  expect_equal(nrow(draw_data(two_arm_trial(N = 5))), 5)
  expect_equal(nrow(draw_data(two_arm_trial(N = 15))), 15)

  a_expand_design <- expand_design(designer = two_arm_trial, N = 50)

  df <- draw_data(a_expand_design)

  expect_equal(nrow(df), 50)
})


test_that("vary works", {
  two_arm_trial <- function(N, noise_sd) {
    my_population <-
      declare_population(N = N, noise = rnorm(N, sd = noise_sd))
    my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    my_assignment <- declare_assignment(m = N / 2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <-
      declare_estimator(Y ~ Z, estimand = pate, label = "pate_hat")
    reveal_outcomes <- declare_reveal()
    my_design <- my_population +
      my_potential_outcomes +
      pate +
      my_assignment +
      reveal_outcomes +
      pate_estimator
    return(my_design)
  }

  design <- expand_design(
    designer = two_arm_trial,
    N = c(100, 200, 300),
    noise_sd = 1
  )
  expect_length(design, 3)
  diagnose_design(design, sims = 2, bootstrap_sims = FALSE)

  design <- expand_design(
    designer = two_arm_trial,
    N = c(100, 200, 300),
    noise_sd = c(.1, .2, .3)
  )
  expect_length(design, 9)
  diagnose_design(design, sims = 2, bootstrap_sims = FALSE)

  design <- expand_design(
    designer = two_arm_trial,
    expand = FALSE,
    N = c(100, 200, 300),
    noise_sd = c(.1, .2, .3)
  )
  expect_length(design, 3)
  diagnose_design(design, sims = 2, bootstrap_sims = FALSE)

  expect_error(expand_design(
    designer = two_arm_trial,
    expand = FALSE,
    N = c(100, 200, 300),
    noise_sd = c(.1, .2)
  ))
})

test_that("power curve", {
  two_arm_trial <- function(N) {
    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + .25)
    my_assignment <- declare_assignment(m = N / 2)
    pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
    pate_estimator <-
      declare_estimator(Y ~ Z, estimand = pate, label = "pate_hat")
    reveal_outcomes <- declare_reveal()
    my_design <- my_population +
      my_potential_outcomes +
      pate +
      my_assignment +
      reveal_outcomes +
      pate_estimator
    return(my_design)
  }

  design <-
    expand_design(designer = two_arm_trial, N = c(100, 200, 300, 500, 1000))

  expect_length(design, 5)

  diagnosis <-
    diagnose_design(design, sims = 2, bootstrap_sims = FALSE)
  #
  #   library(ggplot2)
  #   ggplot(get_diagnosands(diagnosis), aes(x = N, y = power)) +
  #     geom_point() +
  #     geom_line() +
  #     theme_bw()
  #
})

test_that("single design can be created by expand_design", {
  # need to fix this population business single step
  my_designer <-
    function(N = 10) {
      pop <-
        declare_population(N = N)
      design <- pop + NULL
      design
    }

  my_design <- expand_design(my_designer)

  expect_s3_class(my_design, "design")
  #
  # my_design <- expand_design(my_designer, N = 50)
  #
  # expect_s3_class(my_design, "design")
  #
  # expect_equal(nrow(draw_data(my_design)), 50)
  #
  # my_designs <- expand_design(my_designer, N = c(50, 100))
  #
  # expect_equal(length(my_designs), 2)
})
