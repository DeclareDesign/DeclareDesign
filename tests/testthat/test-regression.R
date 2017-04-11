
test_that("regression from DDestimate works as an estimator", {

  library(DDestimate)

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 100)

  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = pate)

  pate_estimator <- declare_estimator(Y ~ Z + noise,
                                      estimator_function = lm_robust_se,
                                      coefficient_name = "noise",
                                      estimand = pate, label = pate)

  my_design <- declare_design(my_population(),
                              my_potential_outcomes,
                              pate,
                              my_assignment,
                              reveal_outcomes,
                              pate_estimator)

  my_design$design_function()

  diagnosis <- diagnose_design(my_design, sims = 10)
  diagnosis

})
