context("Multiple POs")

test_that("multiple potential outcomes", {

  ## would be nice to do with fixed POs

  my_population <- declare_population(
    N = 100, income = rnorm(N), age = sample(18:95, N, replace = T))

  my_potential_outcomes_Y <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z)

  my_potential_outcomes_attrition <- declare_potential_outcomes(
    formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))

  my_assignment <- declare_assignment(m = 25)

  my_design <- declare_design(my_population,
                              my_potential_outcomes_Y,
                              my_potential_outcomes_attrition,
                              my_assignment,
                              declare_reveal(outcome_variable = "R", assignment_variable = "Z"),
                              declare_reveal(outcome_variable = "Y", assignment_variable = "Z")) # Not declared with attrition, so R is normal

  expect_true(all(
    c("R","Y") %in% colnames(draw_data(my_design))
  ))

})
