context("print")

N <- 500

my_population <- declare_population(N = N, noise = rnorm(N))

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_sampling <- declare_sampling(legacy = FALSE, S = complete_rs(N, n = 250))

my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 25))

my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

my_reveal <- declare_reveal()

design <- my_population +
  my_potential_outcomes +
  my_sampling +
  my_inquiry +
  declare_step(dplyr::mutate, q = 5) +
  declare_step(dplyr::mutate, q = 6) +
  my_assignment +
  my_reveal +
  my_estimator

test_that("print code works", {
  expect_output(print_code(design), "my_population <- declare")

  attr(design, "code") <- "code"

  expect_output(print_code(design), "code")
})


# test_that("print full design", {
#
#   expect_equal(capture.output(print_code(design)),
#                c("my_population <- declare_population(N = N, noise = rnorm(N)) ",
#                  "", "my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)) ",
#                  "", "my_sampling <- declare_sampling(legacy = FALSE, S = complete_rs(N, n = 250)) ", "", "my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) ",
#                  "", "my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 25)) ", "", "my_reveal <- declare_reveal() ",
#                  "", "my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry) ",
#                  "", "my_design <- my_population + my_potential_outcomes + my_sampling + my_inquiry + dplyr::mutate(q = 5) + dplyr::mutate(q = 6) + my_assignment + my_reveal + my_estimator) ",
#                  ""))
#
# })

test_that("print a step", {
  expect_equal(capture.output(print(my_reveal)), "declare_reveal()")
})
