

test_that("PO as a formula works", {

  my_population <- declare_population(
    N = 100,
    income = rnorm(N),
    age = sample(18:95, N, replace = T)
  )

  pop <- my_population()

  my_potential_outcomes <-
    declare_potential_outcomes(
      formula = R ~ rbinom(n = N, size = 1, prob = pnorm(.025 * Z)
    ))

  ##debugonce(DeclareDesign:::potential_outcomes_function_formula)
  head(my_potential_outcomes(pop))

})
