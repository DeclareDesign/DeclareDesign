context("Factorial Design")

test_that("Factorial", {
  my_population <- declare_population(N = 2000, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(
    Y_Z_T1 = noise,
    Y_Z_T2 = noise + 0.2,
    Y_Z_T3 = noise + 0.2,
    Y_Z_T4 = noise + 0.6
  )

  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, num_arms = 4))

  my_inquiry <- declare_inquiry(interaction = mean(Y_Z_T4 - Y_Z_T3) - mean(Y_Z_T2 - Y_Z_T1))

  my_estimator <- declare_estimator(Y ~ Z1 + Z2 + Z1 * Z2,
    model = lm_robust,
    term = "Z1:Z2"
  )

  declare_reveal <- declare_reveal()

  my_design <-
    my_population +
    my_potential_outcomes +
    my_inquiry +
    my_assignment +
    declare_step(dplyr::mutate,
      Z1 = as.numeric(Z %in% c("T2", "T4")),
      Z2 = as.numeric(Z %in% c("T3", "T4"))
    ) +
    declare_reveal +
    my_estimator

  expect_equal(my_design %>% draw_data() %>% nrow(), 2000)
  expect_equal(my_design %>% run_design() %>% names(), c("inquiry_label", "estimand", "estimator_label", "term", "estimate", 
                                                         "std.error", "statistic", "p.value", "conf.low", "conf.high", 
                                                         "df", "outcome"))

  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap_sims = FALSE)

  expect_equal(diagnosis %>% get_simulations %>% dim, c(2, 14))

  expect_equal(diagnosis %>%  get_diagnosands %>% dim, c(1, 14))

})
