context("Reshape Diagnosis")

N <- 500

my_population <- declare_model(N = N, noise = rnorm(N))

my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_sampling <- declare_sampling(S = complete_rs(N, n = 250))

my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

design <- my_population +
  my_potential_outcomes +
  my_sampling +
  my_inquiry +
  declare_step(fabricate, q = 5) +
  my_assignment +
  my_measurement +
  my_estimator

test_that("reshape works", {
  set.seed(5)
  dx <- diagnose_design(design, sims = 10, bootstrap_sims = 5)
  reshape_diagnosis(dx)
  expect_error(reshape_diagnosis(dx, select = "mean_estimand"),
    regexp = "select argument must only include elements from"
  )
  reshape_diagnosis(dx, select = "Mean Estimand")
})


test_that("capitalization of parameter names are retained", {
  my_designer <- function(N = 100, n = 50) {
    my_pop <- declare_model(N = N, noise = rnorm(N))
    my_pos <-
      declare_potential_outcomes(
        Y_Z_0 = noise,
        Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
      )
    my_smp <- declare_sampling(S = complete_rs(N, n = n))
    my_asgn <- declare_assignment(Z = complete_ra(N, m = floor(n / 2)))
    my_inquiry <- declare_inquiry(mean(Y_Z_1) - mean(Y_Z_0))
    my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
    my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
    my_design <- my_pop + my_pos + my_inquiry + my_smp + my_asgn + my_measurement + my_estimator
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

test_that("reshaping without estimators", {
  design <- 
    declare_model(N = 2) + 
    declare_inquiry(Q = 1) 
  
  expect_error(
    design |>
    diagnose_design(sims = 2) |>
    reshape_diagnosis(),
    NA)
  
})



test_that("select", {
  dx <- diagnose_design(design, sims = 10, bootstrap_sims = 5)
  reshape <- reshape_diagnosis(dx, select = "Bias")
  expect_equal(colnames(reshape), c("Design", "Inquiry", "Estimator", "Outcome", "Term", "N Sims", "Bias"))
})


test_that("designs with factors in diagnosands_df do not produce warnings", {
  
  my_estimator <- function(data) {
    data.frame(estimate = c("answer1", "answer2"), stringsAsFactors = TRUE)
  }

  design <- design <- my_population +
    declare_estimator(handler = label_estimator(my_estimator), label = "my_label")

  diagnose_design(design, sims = 2, diagnosands = declare_diagnosands(first = estimate[1]))
  
  my_estimator <- function(data) {
    data.frame(estimate = c("answer1", "answer2"), estimator = "my_label", stringsAsFactors = TRUE)
  }
  
  design <- design <- my_population +
    declare_estimator(handler = my_estimator)
  
  expect_silent(reshape_diagnosis(diagnose_design(design, sims = 31, diagnosands = declare_diagnosands(first = estimate[1]))))
  
})

test_that("groups with factors", {
  
  skip_on_cran()
  
  set.seed(17)
  design <- 
    declare_model(N = 100, u = rnorm(N)) + 
    declare_model(Y_Z_0 = 0, Y_Z_1 = ifelse(rbinom(N, 1, prob = 0.5), 0.1, -0.1) + u) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_inquiry(ATE_positive = mean(Y_Z_1 - Y_Z_0) > 0) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE_positive")
  
  expect_warning(expect_equal(
    diagnose_design(design, 
                    make_groups = vars(significant = ifelse(p.value > 0.5, NA, p.value <= 0.05)),
                    sims = 5
    )$diagnosands_df$significant,
    c(FALSE, NA)
  ))
    
  expect_warning(expect_equal(
    diagnose_design(design, 
                    make_groups = vars(significant = factor(ifelse(p.value > 0.5, NA, p.value <= 0.05))),
                    sims = 5
    )$diagnosands_df$significant,
    structure(c(1L, NA), .Label = "FALSE", class = "factor")
  ))
  
})
