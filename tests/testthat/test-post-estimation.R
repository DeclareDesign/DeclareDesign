context("post estimation")

test_that("multiple design get_estimates", {
  my_designer <- function(N = 50) {
    my_population <- declare_population(N = N, noise = rnorm(N))

    my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_assignment <- declare_assignment(m = 25)

    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

    my_reveal <- declare_reveal()

    my_design <-
      my_population +
      my_potential_outcomes +
      my_estimand +
      my_assignment +
      my_reveal +
      my_estimator

    my_design
  }

  my_population <- declare_population(N = 100, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  my_reveal <- declare_reveal()

  design_1 <- my_population +
    my_potential_outcomes +
    my_estimand +
    my_assignment +
    my_reveal +
    my_estimator

  my_assignment_2 <- declare_assignment(m = 50)

  design_2 <- replace_step(design_1, my_assignment, my_assignment_2)

  my_designs <- expand_design(my_designer, N = c(50, 100))

  get_estimands(design_1)

  get_estimands(design_2)

  get_estimands(design_1, design_2)

  get_estimands(my_designs)

  get_estimates(design_1)

  get_estimates(design_2)

  expect_equal(get_estimates(design_1, design_2)$design_label, c("design_1", "design_2"))

  get_estimates(my_designs)
})
