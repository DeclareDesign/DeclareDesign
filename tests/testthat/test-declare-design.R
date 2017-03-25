rm(list = ls())
library(magrittr)
library(dplyr)
library(DeclareDesign)
library(DDfabricate)
library(DDestimate)
library(randomizr)

my_population <- declare_population(N = 500, noise = rnorm(N))

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_sampling <- declare_sampling(n = 250)

my_assignment <- declare_assignment(m = 25)

my_estimator <- declare_estimator(Y ~ Z)

my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

my_population() %>% my_potential_outcomes %>% my_sampling %>% my_assignment %>% reveal_outcomes %>% my_estimator
#debugonce(declare_estimand)

#debugonce(my_estimand)


#debugonce(design$design_function)
design <- declare_design(my_population(),
                         my_potential_outcomes,
                         my_estimand,
                         my_sampling,
                         my_assignment,
                         reveal_outcomes,
                         my_estimator)

design$data_function() %>% head
design$design_function()

diagnose_design(design = design, diagnosands = declare_diagnosands(superpower = mean(p < .555)))




