context("Reveal Outcomes")





test_that("Reveal Outcomes", {
  my_population <- declare_model(N = 1000, u = rnorm(N))

  my_sampling <- declare_sampling(S = complete_rs(N, n = 100))

  my_assignment <- declare_assignment(Z = complete_ra(N, m = 50))

  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = u,
      Y_Z_1 = u + .25
    )

  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

  my_design <- my_population + my_potential_outcomes + my_inquiry + my_sampling + my_assignment + my_measurement + my_estimator

  dat <- draw_data(my_design)


  expect_true(
    all(vapply(my_design, function(step) "design_step" %in% class(step), FALSE)),
    "all steps should have appropriate class set"
  )
})

test_that("Reveal Outcomes NSE for assignment / outcome variables ", {
  my_population <- declare_model(N = 500, noise = 1:N)

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 1)

  my_assignment <- declare_assignment(Z = complete_ra(N, prob = 1))
  
  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

  design <- my_population + my_potential_outcomes + my_assignment + my_measurement

  df1 <- draw_data(design)


  design <- my_population + my_potential_outcomes + my_assignment +
    declare_measurement(Y = reveal_outcomes(Y ~ Z))

  df2 <- draw_data(design)

  design <- my_population + my_potential_outcomes + my_assignment +
    declare_measurement(Y = reveal_outcomes(Y ~ Z))

  df3 <- draw_data(design)

  expect_identical(df1, df2)
  expect_identical(df1, df3)
})

test_that("reveal multiple outcomes works", {
  N <- 25

  my_population <- declare_model(N = N, noise = 1:N)
  my_potential_outcomes1 <- declare_potential_outcomes(formula = Y1 ~ Z * .25, conditions = c(0, 1))
  my_potential_outcomes2 <- declare_potential_outcomes(formula = Y2 ~ Z * .5 + 3, conditions = c(0, 1))
  my_assignment <- declare_assignment(Z = complete_ra(N, prob = 1))

  design <- my_population + my_potential_outcomes1 + my_potential_outcomes2 + my_assignment +
    declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z), Y2 = reveal_outcomes(Y2 ~ Z)) 
  df1 <- draw_data(design)

  design <- my_population + my_potential_outcomes1 + my_potential_outcomes2 + my_assignment +
    declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z), Y2 = reveal_outcomes(Y2 ~ Z)) 
  df2 <- draw_data(design)

  expect_identical(df1, df2)
})

test_that("declare_reveal custom handler works", {
  N <- 25

  my_population <- declare_model(N = N, noise = rnorm(N))
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 10))

  my_outcome_function <- function(data) {
    data$Y <- rnorm(nrow(data))
    return(data)
  }

  design <- my_population + my_assignment + declare_reveal(handler = my_outcome_function)
  df <- draw_data(design)

  expect_true("Y" %in% colnames(df))
})

test_that("missing PO stops", {
  expect_error(
    declare_measurement(foo = reveal_outcomes(foo ~ extra))(sleep)
  )
})

test_that("Not all Potential outcome columns present", {
  set.seed(4)
  df <- data.frame(Z = sample(1:3, 100, replace = TRUE), Y_Z_0 = 1:100, Y_Z_1 = 1:100)

  expect_error(
    declare_measurement(Y = reveal_outcomes(Y ~ Z))(df),
    "Y_Z_3"
  )
})


test_that("Single outcome, multiple assn", {
  population <- declare_model(
    blocks = fabricatr::add_level(
      N = 40,
      block_shock = rnorm(N)
    ),
    subjects = fabricatr::add_level(
      N = 8,
      shock = rnorm(N) + block_shock
    )
  )

  # Model ----------------------------------------------------------------------
  potential_outcomes <- declare_potential_outcomes(
    Y_A_0_B_0 = 1, Y_A_1_B_0 = 2,
    Y_A_0_B_1 = 3, Y_A_1_B_1 = 4
  )


  # Factorial assignments
  assign_A <- declare_assignment(
    A = block_ra(blocks = blocks)
  )
  assign_B <- declare_assignment(
    B = block_ra(blocks = A + 10 * as.numeric(blocks))
  )

  design <- population + potential_outcomes + assign_A + assign_B + declare_measurement(Y = reveal_outcomes(Y ~ A + B))

  dd <- draw_data(design)

  expect_equal(c(table(dd$Y)), c(80, 80, 80, 80), check.attributes = FALSE)
})


test_that("handler rrror on symbols", {
  expect_error(reveal_outcomes_handler(sleep, outcome_variables = as.symbol("Y")))
  expect_error(reveal_outcomes_handler(sleep,
    outcome_variables = "Y",
    assignment_variables = as.symbol("Z")
  ))
  expect_error(reveal_outcomes_handler(sleep,
    outcome_variables = "Y",
    assignment_variables = "Z",
    attrition_variables = as.symbol("R")
  ))
})

