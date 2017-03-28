# rm(list = ls())
# library(testthat)
# library(DeclareDesign)
# library(randomizr)
#
# ##context("Checking Code in Paper Works")
#
# # “Characterizing Research Designs in Code" -------------------------------
#
# ##test_that("section on 'Characterizing Research Designs in Code' works", {
#   my_population <- function(size) {
#     data.frame(u = rnorm(size))
#   }
#   population <-
#     declare_population(population_function = my_population, size = 100)
#   population()
#
#   my_sampling <- function(data) {
#     rbinom(n = nrow(data),
#            size = 1,
#            prob = 0.1)
#   }
#   sampling <- declare_sampling(sampling_function = my_sampling)
#   my_assignment <- function(data) {
#     rbinom(n = nrow(data),
#            size = 1,
#            prob = 0.5)
#   }
#   assignment <-
#     declare_assignment(assignment_function = my_assignment,
#                        condition_names = c(0, 1))
#   my_potential_outcomes <-
#     function(data) {
#       with(data, Z * 0.25 + u)
#     }
#   potential_outcomes <- declare_potential_outcomes(
#     potential_outcomes_function = my_potential_outcomes,
#     outcome_variable_name = 'Y',
#     condition_names = c(0, 1)
#   )
#   my_estimand <- function(data) {
#     with(data, mean(Y_Z_1 - Y_Z_0))
#   }
#   estimand <- declare_estimand(estimand_function = my_estimand,
#                                potential_outcomes = potential_outcomes)
#
#   my_estimates <- function(data) {
#     reg <- lm(Y ~ Z, data = data)
#     phi <- as.list(summary(reg)$coefficients["Z", ])
#     c(
#       est = phi$Estimate,
#       se = phi$"Std. Error",
#       p = phi$"Pr(>|t|)"
#     )
#   }
#   estimator <-
#     declare_estimator(estimates = my_estimates, estimand = estimand)
#
#   bias_diagnosand <- "est - estimand"
#
#   diagnosand <-
#     declare_diagnosand(diagnostic_statistic_text = bias_diagnosand,
#                        summary_function = mean)
#
#   design <-
#     declare_design(
#       population = population,
#       sampling = sampling,
#       assignment = assignment,
#       potential_outcomes = potential_outcomes,
#       estimator = estimator,
#       diagnosand = diagnosand
#     )
#
#
#   diagnose_design(
#     design = design,
#     population_draws = 10,
#     sample_draws = 1,
#     assignment_draws = 1,
#     bootstrap_diagnosands = F
#   )
#
# ##})
