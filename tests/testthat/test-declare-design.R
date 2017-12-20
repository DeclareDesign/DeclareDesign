context("declare design")

test_that("test the full declare design setup", {

  N <- 500

  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_assignment <- declare_assignment(m = 25)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_sampling,
                           my_estimand,
                           dplyr::mutate(q = 5),
                           dplyr::mutate(q = 6),
                           my_assignment,
                           reveal_outcomes,
                           my_estimator)

  head(draw_data(design))
  execute_design(design)

  #summary(design)

  diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)

})


test_that("test the full declare design setup", {

  design <- declare_design(declare_population(N = 500, noise = rnorm(N)),
                           declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)),
                           declare_sampling(n = 250),
                           declare_assignment(m = 25),
                           reveal_outcomes)

  head(draw_data(design))
  execute_design(design)

  #diagnose_design(design, sims = 3) - # will fail, no estimands/estimates are declared

})



