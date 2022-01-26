context("declare design")

test_that(
  "test the full declare design setup", {
    N <- 500

    my_population <- declare_model(N = N, noise = rnorm(N))

    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_sampling <- declare_sampling(S = complete_rs(N, n = 250))

    my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

    my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

    my_mutate <- declare_step(dplyr::mutate, noise_sq = noise^2)

    my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

    design <- my_population +
      my_potential_outcomes +
      my_sampling +
      my_inquiry +
      my_mutate +
      my_assignment +
      my_measurement +
      my_estimator

    df <- (draw_data(design))
    expect_equal(dim(df), c(250, 8))

    output <- run_design(design)
    expect_equal(dim(output), c(1, 12))
  }
)


test_that("No estimators / inquiries", {
  design <-
    declare_model(N = 500, noise = 1:N, Y_Z_0 = noise, Y_Z_1 = noise + 1) +
    declare_sampling(S = complete_rs(N, n = 250)) +
    declare_assignment(Z = complete_ra(N, m = 25)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

  head(draw_data(design))
  expect_error(run_design(design), "No estimates or inquiries were declared")
})

test_that("single-step designs work", {
  pop <- declare_model(N = 100)

  des_1 <- +pop
  des_2 <- pop + NULL

  expect_equal(des_1, des_2)
})

test_that("sending bad objects to design yields error", {
  pop <- declare_model(N = 100)

  my_func <- function(x) {
    return(x)
  }

  # can't send a function call
  expect_error(pop + my_func(5), "The right hand side")
})

test_that("test send design as RHS", {
  my_rhs <- declare_sampling(S = complete_rs(N, n = 50)) + declare_assignment(Z = complete_ra(N, m = 5))

  expect_length(declare_model(N = 100) + my_rhs, 3)
})


test_that("send function that doesn't have data as first arg sends warning", {
  my_func <- function(my_arg) return(my_arg)

  expect_warning(declare_model(N = 100) + my_func, "Undeclared Step 2 function arguments are not exactly 'data'")
})

