context("Continuous POs")
test_that("you can do continuous POs", {

  my_population <- declare_population(
    N = 100, income = rnorm(N), age = sample(18:95, N, replace = T))

  conditions = seq(0, 1, by = .1)

  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z, conditions = conditions)

  my_assignment <- declare_assignment(conditions=conditions)

  my_design <- declare_design(my_population,
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes)




  df <- head(draw_data(my_design))

  expect_length(colnames(df) %i% paste("Y", "Z", conditions, sep="_"), 11)



})
