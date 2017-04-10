test_that("test the full declare design setup", {

  library(DeclareDesign)

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  # Way 1

  design <- declare_design(my_population, my_potential_outcomes)
  design$data_function()

  # Way 2

  design <- declare_design(my_population(), my_potential_outcomes)
  design$data_function()

  # Way 3

  df <- my_population()
  design <- declare_design(df, my_potential_outcomes)
  design$data_function()

}
