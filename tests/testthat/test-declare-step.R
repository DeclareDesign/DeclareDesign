context("declare step")

test_that("test declare step ", {

  N <- 50
  my_population <- declare_population(N = N, noise = rnorm(N))
  my_assignment <- declare_assignment(m = 25)
  my_step <- declare_step(Z2 = Z, q = 5)

  design <- declare_design(my_population,
                           my_assignment,
                           my_step)
  design
})
