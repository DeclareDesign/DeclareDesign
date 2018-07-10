context("general label issues")

test_that("error if more than one label is sent to any step", {
  
  expect_error(declare_population(
    N = 5,
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_potential_outcomes(
    Y ~ Z,
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_sampling(
    n = 10,
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_assignment(
    m = 10,
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_reveal(
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_estimator(
    Y ~ A + B,
    model = lm_robust,
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_estimand(
    mean(Y),
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
  expect_error(declare_diagnosands(
    mean(Y),
    label = c("lbl1", "lbl2")), "Please provide only one label.")
  
})
