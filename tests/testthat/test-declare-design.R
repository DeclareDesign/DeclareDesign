context("declare design")

test_that(
  "test the full declare design setup", {
    N <- 500

    my_population <- declare_population(N = N, noise = rnorm(N))

    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_sampling <- declare_sampling(n = 250)

    my_assignment <- declare_assignment(m = 25)

    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

    my_mutate <- declare_step(dplyr::mutate, noise_sq = noise^2)

    my_reveal <- declare_reveal()

    design <- my_population +
      my_potential_outcomes +
      my_sampling +
      my_estimand +
      my_mutate +
      my_assignment +
      my_reveal +
      my_estimator

    df <- (draw_data(design))
    expect_equal(dim(df), c(250, 9))

    output <- run_design(design)
    expect_equal(dim(output$estimates_df), c(1, 11))
    expect_equal(dim(output$estimands), c(1, 2))
  }
)


test_that("No estimators / estimands", {
  design <-
    declare_population(N = 500, noise = 1:N) +
    declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 1) +
    declare_sampling(n = 250) +
    declare_assignment(m = 25) +
    declare_reveal()

  head(draw_data(design))
  expect_identical(
    run_design(design),
    structure(list(
      estimates_df = structure(list(), class = "data.frame", row.names = integer(0)),
      estimands_df = structure(list(), class = "data.frame", row.names = integer(0))
    ), .Names = c(
      "estimates_df",
      "estimands_df"
    ))
  )
})

test_that("single-step designs work", {
  pop <- declare_population(N = 100)

  des_1 <- +pop
  des_2 <- pop + NULL

  expect_equal(des_1, des_2)
})

test_that("sending bad objects to design yields error", {
  pop <- declare_population(N = 100)

  my_func <- function(x) {
    return(x)
  }

  # can't send a function call
  expect_error(pop + my_func(5), "The right hand side")
})

test_that("test send design as RHS", {
  my_rhs <- declare_sampling(n = 50) + declare_assignment(m = 5)

  expect_length(declare_population(N = 100) + my_rhs, 3)
})


test_that("send function that doesn't have data as first arg sends warning", {
  my_func <- function(my_arg) return(my_arg)

  expect_warning(declare_population(N = 100) + my_func, "Undeclared Step 2 function arguments are not exactly 'data'")
})
