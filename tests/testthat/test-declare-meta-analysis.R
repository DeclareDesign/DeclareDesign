context("declare design")

test_that("test the full declare design setup", {

  library(estimatr)
  library(dplyr)
  N <- 500

  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_assignment <- declare_assignment(m = 25)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  design1 <- declare_design(my_population,
                           my_potential_outcomes,
                           my_sampling,
                           my_estimand,
                           dplyr::mutate(q = 5),
                           my_assignment,
                           reveal_outcomes,
                           my_estimator)

  design2 <- declare_design(my_population,
                           my_potential_outcomes,
                           my_sampling,
                           my_estimand,
                           dplyr::mutate(q = 25),
                           my_assignment,
                           reveal_outcomes,
                           my_estimator)

  my_meta_estimator <- declare_estimator(est ~ 1,
                                         estimator_function = lm_robust_se,
                                         coefficient_name = "(Intercept)")

  metad <- declare_meta_analysis(designs = list(design1, design2),
                        type = estimates, my_meta_estimator)

  debugonce(metad$design_function)

  head(metad$data_function())
  metad$design_function()

  #debugonce(diagnose_design)
  #debugonce(DeclareDesign:::diagnose_design_single_design)
  diagnosis <- diagnose_design(design, sims = 3)


})




