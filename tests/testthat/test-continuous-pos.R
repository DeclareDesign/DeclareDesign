context("Continuous POs")
test_that("you can do continuous POs", {

  my_population <- declare_population(
    N = 100, income = rnorm(N), age = sample(18:95, N, replace = T))

  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z, condition_names = seq(0, 1, by = .1))

  continuous_treatment_function <- function(data){
    data$Z <- sample(seq(0, 1, by = .1), size = nrow(data), replace = TRUE)
    data
  }

  my_assignment <- declare_assignment(handler = continuous_treatment_function)

  my_design <- declare_design(my_population,
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes)

  head(draw_data(my_design))

})
