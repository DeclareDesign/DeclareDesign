context("Custom Functions")

test_that("custom population", {
  ## population
  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler = my_population_function, N = 100
  )

  rm(my_population_function)
  pop_custom <- my_population_custom()

  expect_equal(dim(pop_custom), c(100, 1))
})

test_that("custom PO", {
  ## potential outcomes
  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, extra)
      data$Y_Z_1 <- with(data, 0.25 + extra)
      data
    }

  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler = my_potential_outcomes_function
  )

  rm(my_potential_outcomes_function)
  pop_custom <- my_potential_outcomes_custom(data = sleep)

  expect_equal(dim(pop_custom), c(20, 5))
})


test_that("custom sampling", {
  ## sampling
  my_sampling_function <- function(data) {
    data$S <- rbinom(
      n = nrow(data),
      size = 1,
      prob = 0.1
    )
    data[data$S == 1, ]
  }

  my_sampling_custom <- declare_sampling(
    handler = my_sampling_function
  )

  smp_custom <- my_sampling_custom(sleep)
  expect_true("S" %in% names(smp_custom))
})
