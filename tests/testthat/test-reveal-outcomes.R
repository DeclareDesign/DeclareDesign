context("Reveal Outcomes")





test_that("Reveal Outcomes", {
  my_population <- declare_population(N = 1000, u = rnorm(N))

  my_sampling <- declare_sampling(legacy = FALSE, S = complete_rs(N, n = 100))

  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 50))

  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = u,
      Y_Z_1 = u + .25
    )

  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

  my_reveal <- declare_reveal()

  my_design <- my_population + my_potential_outcomes + my_inquiry + my_sampling + my_assignment + my_reveal + my_estimator

  dat <- draw_data(my_design)


  expect_true(
    all(vapply(my_design, function(step) "design_step" %in% class(step), FALSE)),
    "all steps should have appropriate class set"
  )
})

test_that("Reveal Outcomes NSE for assignment / outcome variables ", {
  my_population <- declare_population(N = 500, noise = 1:N)

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 1)

  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, prob = 1))

  design <- my_population + my_potential_outcomes + my_assignment + declare_reveal()

  df1 <- draw_data(design)


  design <- my_population + my_potential_outcomes + my_assignment +
    declare_reveal(assignment_variables = Z, outcome_variables = Y)

  df2 <- draw_data(design)

  design <- my_population + my_potential_outcomes + my_assignment +
    declare_reveal(assignment_variable = "Z", outcome_variable = "Y")

  df3 <- draw_data(design)

  expect_identical(df1, df2)
  expect_identical(df1, df3)
})

test_that("reveal multiple outcomes works", {
  N <- 25

  my_population <- declare_population(N = N, noise = 1:N)
  my_potential_outcomes1 <- declare_potential_outcomes(formula = Y1 ~ Z * .25, conditions = c(0, 1))
  my_potential_outcomes2 <- declare_potential_outcomes(formula = Y2 ~ Z * .5 + 3, conditions = c(0, 1))
  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, prob = 1))

  design <- my_population + my_potential_outcomes1 + my_potential_outcomes2 + my_assignment +
    declare_reveal(outcome_variables = c(Y1, Y2))
  df1 <- draw_data(design)

  design <- my_population + my_potential_outcomes1 + my_potential_outcomes2 + my_assignment +
    declare_reveal(outcome_variables = c("Y1", "Y2"))
  df2 <- draw_data(design)

  expect_identical(df1, df2)
})

test_that("declare_reveal custom handler works", {
  N <- 25

  my_population <- declare_population(N = N, noise = rnorm(N))
  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 10))

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
    declare_reveal(outcome_variables = foo, assignment_variables = extra)(sleep)
  )
})

test_that("Not all Potential outcome columns present", {
  df <- data.frame(Z = sample(1:3, 100, replace = TRUE), Y_Z_0 = 1:100, Y_Z_1 = 1:100)

  expect_error(
    declare_reveal()(df),
    "Y_Z_3"
  )
})


test_that("Single outcome, multiple assn", {
  population <- declare_population(
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
  assign_A <- declare_assignment(legacy = FALSE, 
    A = block_ra(blocks = blocks)
  )
  assign_B <- declare_assignment(legacy = FALSE, 
    B = block_ra(blocks = A + 10 * as.numeric(blocks))
  )

  design <- population + potential_outcomes + assign_A + assign_B + declare_reveal(outcome_variables = Y, assignment_variables = c(A, B))

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
