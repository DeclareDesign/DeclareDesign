context("Factorial Design")

test_that("Factorial", {

  my_population <- declare_population(N = 2000, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_T1 = noise,
                                                      Y_Z_T2 = noise + 0.2,
                                                      Y_Z_T3 = noise + 0.2,
                                                      Y_Z_T4 = noise + 0.6)

  my_assignment <- declare_assignment(num_arms = 4)

  my_estimand <- declare_estimand(interaction = mean(Y_Z_T4 - Y_Z_T3) - mean(Y_Z_T2 - Y_Z_T1))

  my_estimator <- declare_estimator(Y ~ Z1 + Z2 + Z1*Z2,
                                    model = lm_robust,
                                    coefficient_name = "Z1:Z2")

  my_design <-
    declare_design(
      my_population,
      my_potential_outcomes,
      my_estimand,
      my_assignment,
      dplyr::mutate(Z1 = as.numeric(Z %in% c("T2", "T4")),
             Z2 = as.numeric(Z %in% c("T3", "T4"))),
      reveal_outcomes,
      my_estimator
    )

  draw_data(my_design) %>% head
  execute_design(my_design)

  my_design

  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)
  diagnosis$simulations
})
