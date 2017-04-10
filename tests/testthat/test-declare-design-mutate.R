test_that("use custom functions like mutate in the causal order", {

  library(dplyr)

  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_assignment <- declare_assignment(m = 25)

  my_estimator <- declare_estimator(Y ~ Z)

  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

  debugonce(declare_design)
  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_estimand,
                           mutate(q = 99),
                           my_sampling,
                           my_assignment,
                           reveal_outcomes(outcome_variable_name = "Y"),
                           my_estimator)

  head(draw_data(design))

})

##my_population() %>% my_potential_outcomes %>% declare_step(mutate(q = 99))

#
# if (dots_classes[1] == "function") {
#   ## if you send declare_population(population, pos)
#   ## then we need to change it to run population
#   ## population is a function, population() returns a df
#   dots[[1]]$expr <- paste0(deparse(dots[[1]]$expr), "()")
# } else {
#   current_df <- causal_order[[1]]
# }
