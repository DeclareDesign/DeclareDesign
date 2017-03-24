# library(DeclareDesign)
# library(magrittr)
# library(dplyr)
# library(DDestimate)
#
# my_population <- declare_population(N = 500, noise = rnorm(N))
#
# my_sampling <- declare_sampling(n = 150)
#
# my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
#
# my_assignment_1 <- declare_assignment(m = 25)
# my_assignment_2 <- declare_assignment(m = 50)
#
# pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = pate)
# sate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = sate)
#
# pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = pate)
# sate_estimator <- declare_estimator(Y ~ Z, estimand = sate, label = sate)
#
# my_design_1 <- declare_design(my_population(),
#                               my_potential_outcomes,
#                               pate,
#                               mutate(myvar = 5),
#                               my_assignment_1,
#                               reveal_outcomes,
#                               pate_estimator)
#
# my_design_2 <- modify_design(my_design_1, from = my_assignment_1, to = my_assignment_2)
#
#
# diagnosis_1 <- diagnose_design(my_design_1, sims = 10)
# diagnosis_2 <- diagnose_design(my_design_2, sims = 10)
