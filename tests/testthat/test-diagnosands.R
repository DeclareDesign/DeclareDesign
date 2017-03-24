library(dd)
library(magrittr)
library(dplyr)
library(estimatorpack)

my_population <- declare_population(N = 500, noise = rnorm(N))

my_sampling <- declare_sampling(n = 150)

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_assignment <- declare_assignment(m = 25)

pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = test)

my_design <- declare_design(my_population(),
                      my_potential_outcomes, pate,
                      my_assignment,
                      reveal_outcomes(),
                      pate_estimator)

# default set
diagnosis <- diagnose_design(my_design, sims = 10)
diagnosis$diagnosands


# PLEASE NOTE.  sometimes dplyr messes up and you get funciton sd not found.  if so, install dev. version.

my_dig <-  declare_diagnosands(bias = mean(est - estimand), sd_bias = sd(estimand))
diagnosis <- diagnose_design(my_design, sims = 10, diagnosands = my_dig)

diagnosis$simulations %>% head
diagnosis$diagnosands


