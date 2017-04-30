context("declare design")

test_that("test the full declare design setup", {

  library(dplyr)
  N <- 500

  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)
  my_assignment_2 <- declare_assignment(m = 25, assignment_variable_name = "Z2")

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           dplyr::mutate(q = 5),
                           my_assignment)

  test <- modify_design(design, add_step(dplyr::mutate(blah = 6), before = my_potential_outcomes))

  test <- modify_design(design, add_step(dplyr::mutate(blah = 6), after = my_potential_outcomes))

  test <- modify_design(design, replace_step(dplyr::mutate(blah = 10), replace = dplyr::mutate(q = 5)))

  test <- modify_design(design, remove_step(dplyr::mutate(q = 5)))

  #multiples

  test <- modify_design(design, add_step(dplyr::mutate(blah = 6),
                                         my_assignment_2,
                                         after = my_potential_outcomes))

  test <- modify_design(design, replace_step(dplyr::mutate(blah = 10), my_assignment_2, replace = dplyr::mutate(q = 5)))

  test <- modify_design(design, remove_step(dplyr::mutate(q = 5)))
  ##debugonce(modify_design)
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

  modify_design(design, add_step(dplyr::mutate(income = noise^2), after = my_assignment))
  modify_design(design, add_step(dplyr::mutate(income = noise^2), before = my_assignment))

})






