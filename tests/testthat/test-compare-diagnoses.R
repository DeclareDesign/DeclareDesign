context("Compare Diagnoses")

design_a <-
  declare_population(N = 100, u = rnorm(N), X = runif(N, 0, 2)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = u + rnorm(N, .5)) +
  declare_assignment() + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), label = "ATE") +
  declare_reveal() +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "est1")
design_b <- redesign(design_a, prob = 0.1)
set.seed(2000)

comparison <- compare_diagnoses(design_a, design_b)

test_that("compare_diagnoses works", {
  set.seed(2000)
  expect_is(compare_diagnoses(design_a, design_b), "compared_diagnoses")
})



test_that("merge_by_estimator working in compare_diagnoses", {
  
  # 1:1 comparison 
  diagnosis_a <- diagnose_design(design_a, sims = 40, bootstrap_sims = 0)
  design_c <- insert_step(design_a, declare_estimator(Y ~  Z + X, estimand =  "ATE", term = "Z", model = lm_robust, label = "est2"), after = "est1")
  comparison <- compare_diagnoses(design_a, design_c, sims = 40, merge_by_estimator = TRUE)
  n1 <- length(diagnosis_a$diagnosand_names)
  n2 <- nrow(comparison$compared_diagnoses_df)
  expect_equal(n1, n2)
  
  # 1:2
  comparison <- compare_diagnoses(design_a, design_c, sims = 40,  merge_by_estimator = FALSE)
  n2 <- nrow(comparison$compared_diagnoses_df)
  expect_equal(n1*2, n2)
  
  # 2:2
  comparison <- compare_diagnoses(design_c, design_c, merge_by_estimator = FALSE)
  n2 <- nrow(comparison$compared_diagnoses_df)
  expect_equal(n1*4, n2)
})


test_that("compare_diagnoses errors when it should", {
  # bootstrap errors
  expect_error(compare_diagnoses(design_a, design_b, bootstrap_sims = 0))
 
  
  
  # diagnosis_df must contain only one unique design_label
  designer <- function(N) {
    declare_population(N = N, noise = rnorm(N)) +
      declare_potential_outcomes(Y ~ 0.20 * Z + noise) +
      declare_assignment() +
      declare_estimand(ate = mean(Y_Z_1 - Y_Z_0)) +
      declare_estimator(Y ~ Z)
  }
  
  designs <- expand_design(designer , N = c(20, 30))
  expect_error(compare_diagnoses(designs, design_a)) 
  design_c <- designer(N = 30)
  expect_error(compare_diagnoses(design_a, design_c)) 
  design_d <- delete_step(design_c, "estimator")
  expect_warning(compare_diagnoses(design_c, design_d, sims = 40)) 
})


test_that("compare_diagnoses prints", {
  expect_output(DeclareDesign:::print.compared_diagnoses(comparison))
})
