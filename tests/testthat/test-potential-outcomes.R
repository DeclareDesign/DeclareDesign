test_that("declare_potential_outcomes", {

  # set up a population

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    population_function = my_population_function, N = 100)

  rm(my_population_function)
  pop_custom <- my_population_custom()

  head(pop_custom)

  # draw POs for it without arguments

  my_po_function <- function(data) {
    data$Y_Z_0 <- with(data, .25 + u)
    data$Y_Z_1 <- with(data, u)
    data
  }

  ##debugonce(declare_potential_outcomes)
  my_po_custom <- declare_potential_outcomes(
    potential_outcomes_function = my_po_function)

  ##debugonce(my_po_custom)
  rm(my_po_function)
  pop_custom <- my_po_custom(pop_custom)

  head(pop_custom)

  ## draw POs for it with arguments

  my_po_function <- function(data, q) {
    data$Y_Z_0 <- with(data, q + u)
    data$Y_Z_1 <- with(data, u)
    data
  }

  ##debugonce(declare_potential_outcomes)
  my_po_custom <- declare_potential_outcomes(
    potential_outcomes_function = my_po_function, q = rnorm(1))

  ##debugonce(my_po_custom)
  rm(my_po_function)
  pop_custom <- my_po_custom(pop_custom)

  head(pop_custom)

})
