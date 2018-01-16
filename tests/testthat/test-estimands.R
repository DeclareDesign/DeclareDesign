context("Estimands")


test_that("test the estimands", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  ## default labeling
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  my_population() %>% my_potential_outcomes %>% my_estimand


  ## no label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  my_population() %>% my_potential_outcomes %>% my_estimand

  ## manual label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "ATE")
  my_population() %>% my_potential_outcomes %>% my_estimand

  ## custom estimand function
  my_estimand_function <- function(data) {
    with(data, median(Y_Z_1 - Y_Z_0))
  }
  my_estimand_custom <- declare_estimand(
    handler = my_estimand_function, label = "medianTE")

  my_population() %>% my_potential_outcomes %>% my_estimand_custom

})



test_that("declare estimand", {

  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  pop <- my_population()
  pop <- my_potential_outcomes(pop)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  attributes(my_estimand)$label
  my_estimand(pop)

  ###debugonce(declare_estimand)
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  attributes(my_estimand)$label
  my_estimand(pop)

  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "ATE")
  attributes(my_estimand)$label
  my_estimand(pop)

  my_estimand <- declare_estimand(SATT = mean(Y_Z_1 - Y_Z_0), label = "ATE")
  attributes(my_estimand)$label
  my_estimand(pop)

  # set up a population

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler = my_population_function, N = 100)

  rm(my_population_function)
  pop_custom <- my_population_custom()

  # draw POs for it without arguments

  my_po_function <- function(data) {
    data$Y_Z_0 <- with(data, .25 + u)
    data$Y_Z_1 <- with(data, u)
    data
  }

  my_po_custom <- declare_potential_outcomes(
    handler  = my_po_function)

  pop_custom <- my_po_custom(pop_custom)


  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

  my_estimand(pop_custom)

})


test_that("multiple estimand declarations work", {

  population <- declare_population(
      N = 10^3,
      noise = rnorm(N)
  )

  potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ noise + Z*.5)

  pop <- potential_outcomes(population())


  sampling <- declare_sampling(
    n = 100
  )


  assignment <- declare_assignment(
    N = 50
  )

  # No explicit label, should inherit
  sate <- declare_estimand(SATE = mean(Y_Z_1 - Y_Z_0))
  pate <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))
  # Explicit label, should not inherit
  sate_label <- declare_estimand(mean(Y_Z_1 - Y_Z_0),label = "The SATE")
  pate_label <- declare_estimand(mean(Y_Z_1 - Y_Z_0),label = "The PATE")
  # No labeling whatsoever
  sate_nolabel <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  pate_nolabel <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

  estimator <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate,
                      label = "simple")

  design_1 <- declare_design(population,
                           potential_outcomes,
                           pate,
                           sampling,
                           sate,
                           assignment,
                           reveal_outcomes,
                           estimator
  )
  design_2 <-  declare_design(population,
                              potential_outcomes,
                              pate_label,
                              sampling,
                              sate_label,
                              assignment,
                              reveal_outcomes,
                              estimator
  )
  # This could eventually be fixed so that the estimand object names are inherited
  expect_error({
    design_3 <-  declare_design(population,
                                potential_outcomes,
                                pate_nolabel,
                                sampling,
                                sate_nolabel,
                                assignment,
                                reveal_outcomes,
                                estimator
    )
  })

})

