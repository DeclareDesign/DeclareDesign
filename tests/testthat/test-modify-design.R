context("modify design")

test_that("test modify declare design ", {

  library(dplyr)
  N <- 500

  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           dplyr::mutate(q = 5),
                           my_assignment)

  my_assignment_2 <- declare_assignment(m = 25, assignment_variable_name = "Z2")

  replace_step(design, new_step = my_assignment_2, step = my_assignment)

  insert_step(design, dplyr::mutate(blah = 6), before = my_potential_outcomes)

  insert_step(design, dplyr::mutate(blah = 6), after = my_potential_outcomes)

  replace_step(design, dplyr::mutate(blah = 10), step = my_population)

  delete_step(design, 3)

})



test_that("placement doesn't matter", {
  my_population <- declare_population(N = 100, noise = rnorm(N))

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = noise,
                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 50)
  my_assignment_2 <- declare_assignment(m = 25)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment)

  design

  insert_step(design, dplyr::mutate(income = noise^2), after = my_assignment)
  insert_step(design, dplyr::mutate(income = noise^2), before = my_assignment)

})






