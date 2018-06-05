

N <- 500

my_population <- declare_population(N = N, noise = rnorm(N))

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_sampling <- declare_sampling(n = 250)

my_assignment <- declare_assignment(m = 25)

my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

my_reveal <- declare_reveal()

design <- declare_design(my_population,
                         my_potential_outcomes,
                         my_sampling,
                         my_estimand,
                         dplyr::mutate(q = 5),
                         dplyr::mutate(q = 6),
                         my_assignment,
                         my_reveal,
                         my_estimator)

# test_that("print full design", {
# 
#   expect_equal(capture.output(print_code(design)), 
#                c("my_population <- declare_population(N = N, noise = rnorm(N)) ", 
#                  "", "my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)) ", 
#                  "", "my_sampling <- declare_sampling(n = 250) ", "", "my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) ", 
#                  "", "my_assignment <- declare_assignment(m = 25) ", "", "my_reveal <- declare_reveal() ", 
#                  "", "my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand) ", 
#                  "", "my_design <- declare_design(my_population, my_potential_outcomes, my_sampling, my_estimand, dplyr::mutate(q = 5), dplyr::mutate(q = 6), my_assignment, my_reveal, my_estimator) ", 
#                  ""))
# 
# })

test_that("print a step", {
  
  expect_equal(capture.output(print(my_reveal)), "declare_reveal()")
  
})


