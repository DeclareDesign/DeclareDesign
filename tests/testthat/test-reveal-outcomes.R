context("Reveal Outcomes")

test_that("Reveal Outcomes", {


  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           reveal_outcomes)

  head(design$data_function())

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           reveal_outcomes(assignment_variable_name = Z,
                                           outcome_variable_name = Y))

  head(design$data_function())

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           reveal_outcomes(assignment_variable_name = "Z",
                                           outcome_variable_name = "Y"))

  head(design$data_function())

})
