library(DeclareDesign)

my_population <- declare_population(
  N = 100, income = rnorm(N), age = sample(18:95, N, replace = T))

pop <- my_population()

logistic <- function(x) exp(x)/(1+exp(x))

my_potential_outcomes1 <- declare_potential_outcomes(
  formula = R ~ rbinom(n = N, size = 1, prob = logistic(.025 * Z)))

my_potential_outcomes1(pop) %>% head

