context("Attrition")

## would be nice to do with fixed POs

my_population <- declare_population(
  N = 100, income = rnorm(N), age = sample(18:95, N, replace = TRUE)
)

my_potential_outcomes_Y <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z
)

my_assignment <- declare_assignment(m = 25)

test_that("attrition / formula PO", {
  my_potential_outcomes_attrition <- declare_potential_outcomes(
    formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0))
  )


  my_reveal_attrition <- declare_reveal(outcome_variables = "R")
  my_reveal_outcomes <- declare_reveal(outcome_variables = "Y", attrition_variables = "R")

  my_design <-
    my_population +
    my_potential_outcomes_Y +
    my_potential_outcomes_attrition +
    my_assignment +
    my_reveal_attrition +
    my_reveal_outcomes


  out <- head(draw_data(my_design))

  expect_identical(is.na(out$Y), out$R == 0)
})

test_that("attrition / legacy PO", {
  my_potential_outcomes_attrition <- declare_potential_outcomes(
    R_Z_0 = rbinom(n = N, size = 1, prob = pnorm(income)),
    R_Z_1 = rbinom(n = N, size = 1, prob = pnorm(income + .2))
  )

  my_design <-
    my_population +
    my_potential_outcomes_Y +
    my_potential_outcomes_attrition +
    my_assignment +
    declare_reveal(outcome_variables = "R") +
    declare_reveal(attrition_variables = "R")

  out <- head(draw_data(my_design))

  expect_identical(is.na(out$Y), out$R == 0)
})
