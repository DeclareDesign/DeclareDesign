context("Simulate Design")

my_population <- declare_population(N = 50, noise = rnorm(N))
my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
my_assignment <- declare_assignment(m = 25)
my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
my_reveal <- declare_reveal()

my_design_1 <- my_population + my_potential_outcomes + my_estimand + my_assignment + my_reveal + my_estimator

my_design_2 <- my_design_1

test_that("Simulate Design works", {
  sims <- simulate_design(my_design_1, sims = 5)
  expect_equal(nrow(sims), 5)

  sims <- simulate_design(my_design_1, my_design_2, sims = 5)
  expect_equal(nrow(sims), 10)
  expect_true(all(sims$design_label %in% c("my_design_1", "my_design_2")))

  sims <- simulate_design(list(my_design_1, my_design_2), sims = 5)
  expect_equal(nrow(sims), 10)

  expect_true(all(sims$design_label %in% c("design_1", "design_2")))

  sims <-
    simulate_design(a = my_design_1, b = my_design_2, sims = 5)
  expect_true(all(sims$design_label %in% c("a", "b")))
})


my_designer <- function(N, tau) {
  pop <- declare_population(N = N)
  pos <-
    declare_potential_outcomes(Y_Z_0 = rnorm(N), Y_Z_1 = Y_Z_0 + tau)
  my_assignment <- declare_assignment(m = floor(N / 2))
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
  my_reveal <- declare_reveal()
  my_design_1 <- pop + pos + my_estimand + my_assignment + my_reveal + my_estimator
  my_design_1
}

test_that("expand and simulate", {
  my_designs <-
    expand_design(my_designer,
      N = c(10, 50),
      tau = c(5, 1), prefix = "custom_prefix"
    )
  sims <- simulate_design(my_designs, sims = 5)
  expect_equal(nrow(sims), 20)
  expect_true(all(sims$design_label %in% c("custom_prefix_1", "custom_prefix_2", "custom_prefix_3", "custom_prefix_4")))
  expect_true(all(c("N", "tau") %in% colnames(sims)))
})

test_that("wrong number of sims yields error", {
  expect_error(simulate_design(my_design_1, sims = rep(5, 3)), "Please provide sims a scalar or a numeric vector of length the number of steps in designs.")
})

test_that("zero sims yields error", {
  expect_error(simulate_design(my_design_1, sims = -1), "Sims should be >= 1")
})

test_that("dupe designs give error", {
  expect_error(simulate_design(my_design_1, my_design_1), "You have more than one design named my_design_1")
})



test_that("no estimates estimands declared", {
  my_design_noestmand <- delete_step(my_design_1, my_estimand)
  my_design_noestmand <- delete_step(my_design_noestmand, my_estimator)

  expect_error(simulate_design(my_design_noestmand, sims = 2), "No estimates or estimands were declared, so design cannot be simulated.")
})
