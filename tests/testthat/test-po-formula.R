

test_that("PO as a formula works", {
  my_population <- declare_population(
    N = 100,
    income = rnorm(N),
    age = sample(18:95, N, replace = T)
  )

  pop <- my_population()

  ##logistic <- function(x)
  ##  exp(x) / (1 + exp(x))
  ## including logistic below does not work. why?

  my_potential_outcomes1 <- declare_potential_outcomes(formula = R ~ rbinom(
    n = N,
    size = 1,
    prob = pnorm(.025 * Z)
  ))

  head(my_potential_outcomes1(pop))

})
