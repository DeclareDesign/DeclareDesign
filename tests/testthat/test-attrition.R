context("Attrition")

test_that("attrition as a PO", {

  ## would be nice to do with fixed POs

  my_population <- declare_population(
    N = 100, income = rnorm(N), age = sample(18:95, N, replace = TRUE))

  my_potential_outcomes_Y <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z)

  my_potential_outcomes_attrition <- declare_potential_outcomes(
    formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))

  my_assignment <- declare_assignment(m = 25)

  my_reveal_attrition <- declare_reveal(outcome_variables = "R")
  my_reveal_outcomes <- declare_reveal(outcome_variables = "Y", attrition_variables = "R")

  my_design <- declare_design(
    my_population,
    my_potential_outcomes_Y,
    my_potential_outcomes_attrition,
    my_assignment,
    my_reveal_attrition,
    my_reveal_outcomes
  )

  head(draw_data(my_design))



  # Fixed pos


  my_potential_outcomes_attrition <- declare_potential_outcomes(
    R_Z_0 = rbinom(n = N, size = 1, prob = pnorm(income)),
    R_Z_1 = rbinom(n = N, size = 1, prob = pnorm(income + .2)))

  my_design <- declare_design(
    my_population,
    my_potential_outcomes_Y,
    my_potential_outcomes_attrition,
    my_assignment,
    declare_reveal(outcome_variables = "R"),
    declare_reveal(attrition_variables = "R")
  )

  head(draw_data(my_design))



})
