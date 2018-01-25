context("Custom Functions")

test_that("you can use custom functions for each of the six declare steps", {

  ## population
  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler  = my_population_function, N = 100)

  rm(my_population_function)
  pop_custom <- my_population_custom()
  head(pop_custom)

  ## potential outcomes
  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }

  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler  = my_potential_outcomes_function
  )

  rm(my_potential_outcomes_function)
  pop_custom <- my_potential_outcomes_custom(data = pop_custom)

  head(pop_custom)

  ## sampling
  my_sampling_function <- function(data) {
    S <- rbinom(n = nrow(data),
           size = 1,
           prob = 0.1)
    data[S == 1,]
  }

  my_sampling_custom <- declare_sampling(
    handler  = my_sampling_function)

  smp_custom <- my_sampling_custom(pop_custom)
  smp_custom
})

