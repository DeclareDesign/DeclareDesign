context("Compare Diagnoses")

design_a <-
  declare_population(N = 100, u = rnorm(N), X = runif(N, 0, 2)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = u + rnorm(N, .5)) +
  declare_assignment() + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), label = "ATE") +
  declare_reveal() +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "est1")
design_b <- redesign(design_a, prob = 0.1)


test_that("compare_diagnoses works with design objects", {
  set.seed(2000)
  comparison <- compare_diagnoses(design_a, design_b, sims = 40, bootstrap_sims_base = 100, bootstrap_sims_comparison = 100)
 
  expect_is(comparison, "compared.diagnoses")
  

  
  expect_error( compare_diagnoses(design_a, design_b, sims = 40, bootstrap_sims_base = 0))
  expect_error( compare_diagnoses(diagnosis_a$simulations_df, diagnosis_a$simulations_df))
  })



test_that("compare_diagnoses works with diagnosis objects", {

  diagnosis_a <- diagnose_design(design_a, sims = 40, bootstrap_sims = 100)
  diagnosis_b <- diagnose_design(design_b, sims = 40, bootstrap_sims = 10)
  comparison <- compare_diagnoses(diagnosis_a, diagnosis_b)
  expect_is(comparison, "compared.diagnoses")
  
  
  # compare_diagnoses doesn't return column se_2
  diagnosis_b <- diagnose_design(design_b, sims = 10, bootstrap_sims = 0)
  comparison <- compare_diagnoses(diagnosis_a, diagnosis_b)
  df <- comparison$compared.diagnoses_df
  expect_true(!"se_2" %in%  colnames(df))
  
})


test_that("merge_by_estimator working in compare_diagnoses", {
  
  # 1:1 comparison 
  diagnosis_a <- diagnose_design(design_a, sims = 40, bootstrap_sims = 100)
  design_b <- insert_step(design_a, declare_estimator(Y ~  Z + X, estimand =  "ATE", term = "Z", model = lm_robust, label = "est2"), after = "est1")
  diagnosis_b <- diagnose_design(design_b, sims = 40, bootstrap_sims = 100)
  comparison <- compare_diagnoses(design_a, design_b, sims = 40, bootstrap_sims_base = 100, bootstrap_sims_comparison = 100, merge_by_estimator = TRUE)
  n1 <- length(diagnosis_a$diagnosand_names)
  n2 <- nrow(comparison$compared.diagnoses_df)
  expect_equal(n1, n2)
  
  # 1:2
  comparison <- compare_diagnoses(diagnosis_a, diagnosis_b, sims = 40, bootstrap_sims_base = 100, bootstrap_sims_comparison = 100, merge_by_estimator = FALSE)
  n2 <- nrow(comparison$compared.diagnoses_df)
  expect_equal(n1*2, n2)
  
  # 2:2
  comparison <- compare_diagnoses(diagnosis_b, diagnosis_b, merge_by_estimator = FALSE)
  n2 <- nrow(comparison$compared.diagnoses_df)
  expect_equal(n1*4, n2)
  
})


