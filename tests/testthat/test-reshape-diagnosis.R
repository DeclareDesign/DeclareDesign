context("Reshape Diagnosis")

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
  declare_step(dplyr::mutate, q = 5) +
  my_assignment +
  my_reveal +
  my_estimator

test_that("reshape works", {
  dx <- diagnose_design(design, sims = 10, bootstrap_sims = 5)
  reshape_diagnosis(dx)
  expect_error(reshape_diagnosis(dx, select = "mean_estimand"),
    regexp = "select argument must only include elements from"
  )
  reshape_diagnosis(dx, select = "Mean Estimand")
})


test_that("capitalization of parameter names are retained", {
  my_designer <- function(N = 100, n = 50) {
    my_pop <- declare_population(N = N, noise = rnorm(N))
    my_pos <-
      declare_potential_outcomes(
        Y_Z_0 = noise,
        Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
      )
    my_smp <- declare_sampling(n = n)
    my_asgn <- declare_assignment(m = floor(n / 2))
    my_mand <- declare_estimand(mean(Y_Z_1) - mean(Y_Z_0))
    my_estimator <- declare_estimator(Y ~ Z, estimand = my_mand)
    my_design <- my_pop + my_pos + my_mand + my_smp + my_asgn + declare_reveal() + my_estimator
    my_design
  }

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(100, 50), n = c(50, 25), expand = FALSE
    )

  des <- diagnose_design(design_list, sims = 2)

  reshape <- reshape_diagnosis(des)

  expect_equal(names(reshape)[2:3], c("N", "n"))

  des <- diagnose_design(design_list, sims = 2, bootstrap_sims = 0)

  reshape <- reshape_diagnosis(des)

  expect_equal(names(reshape)[2:3], c("N", "n"))
})



test_that("select", {
  dx <- diagnose_design(design, sims = 10, bootstrap_sims = 5)
  reshape <- reshape_diagnosis(dx, select = "Bias")
  expect_equal(colnames(reshape), c("Design Label", "Estimand Label", "Estimator Label", "Term", "N Sims", "Bias"))
})


test_that("designs with factors in diagnosands_df do not produce warnings", {
  
  my_estimator <- function(data) {
    data.frame(estimate = c("answer1", "answer2"))
  }

  design <- design <- my_population +
    declare_estimator(handler = tidy_estimator(my_estimator), label = "my_label")

  diagnose_design(design, sims = 2, diagnosands = declare_diagnosands(first = first(estimate), keep_defaults = FALSE))
  
  my_estimator <- function(data) {
    data.frame(estimate = c("answer1", "answer2"), estimator_label = "my_label")
  }
  
  design <- design <- my_population +
    declare_estimator(handler = my_estimator)
  
  expect_silent(reshape_diagnosis(diagnose_design(design, sims = 2, diagnosands = declare_diagnosands(first = first(estimate), keep_defaults = FALSE))))
  
})
