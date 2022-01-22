context("declare step")

test_that("test declare step ", {
  my_population <- declare_model(N = 50, noise = rnorm(N))
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
  my_step <- declare_step(fabricate, Z2 = Z, q = 5)

  design <- my_population + my_assignment + my_step
  df <- draw_data(design)
  expect_equal(df$Z2, df$Z)
})
