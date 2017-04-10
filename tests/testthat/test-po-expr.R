

test_that("PO as a formula works", {

  library(DeclareDesign)

  N <- 1000

  my_population <- declare_population(
    N = N,
    income = rnorm(N),
    age = sample(18:95, N, replace = T)
  )

  pop <- my_population()

  ##logistic <- function(x)
  ##  exp(x) / (1 + exp(x))
  ## including logistic below does not work. why?

  my_potential_outcomes1 <- declare_potential_outcomes(
    Y_Z_0 = income,
    Y_Z_1 = income + 5)

  head(my_potential_outcomes1(pop))

})
