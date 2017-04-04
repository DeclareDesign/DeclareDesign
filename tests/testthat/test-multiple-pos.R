library(DeclareDesign)

my_population <- declare_population(
  N = 100, income = rnorm(N), age = sample(18:95, N, replace = T))

my_potential_outcomes1 <- declare_potential_outcomes(
  formula = R ~ rbinom(n = N, size = 1, prob = .1))
my_potential_outcomes2 <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z)

my_potential_outcomes_fixed_R <- declare_potential_outcomes(
  R_Z_0 = rbinom(n = N, size = 1, prob = .05),
  R_Z_1 = rbinom(n = N, size = 1, prob = .05))


my_assignment <- declare_assignment(m = 25)
my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = ATE)
my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
my_design <- declare_design(my_population(),
                            my_potential_outcomes1,
                            my_potential_outcomes2,
                            my_estimand,
                            my_assignment,
                            reveal_outcomes,
                            my_estimator_dim)

my_population() %>% my_potential_outcomes_fixed_R %>% head

head(draw_data(my_design))
