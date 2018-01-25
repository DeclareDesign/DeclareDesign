context("Compare Designs")
test_that("compare_designs works", {

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  pate <- declare_estimand(pate = mean(Y_Z_1 - Y_Z_0))
  sate <- declare_estimand(sate = mean(Y_Z_1 - Y_Z_0))

  pate_estimator <- declare_estimator(Y ~ Z, estimand = pate)
  sate_estimator <- declare_estimator(Y ~ Z, estimand = sate)

  reveal <- declare_reveal()

  my_design_1 <- declare_design(my_population,
                                my_potential_outcomes,
                                pate,
                                my_assignment,
                                reveal,
                                pate_estimator)

  my_design_2 <- declare_design(my_population,
                                my_potential_outcomes,
                                sate,
                                my_assignment,
                                reveal,
                                sate_estimator)


  diagnosis_1 <- diagnose_design(my_design_1, sims = 2, bootstrap = FALSE, parallel = FALSE)
  diagnosis_2 <- diagnose_design(my_design_2, sims = 2, bootstrap = FALSE, parallel = FALSE)

  comparison <- diagnose_design(my_design_1, my_design_2, sims = 2, bootstrap = FALSE, parallel = FALSE)
  comparison

  comparison <- diagnose_design(first_design = my_design_1, my_design_2, sims = 2, bootstrap = FALSE, parallel = FALSE)
  comparison

})
