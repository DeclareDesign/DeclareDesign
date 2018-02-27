context("Reveal Outcomes")





test_that("Reveal Outcomes", {

  my_population <- declare_population(N = 1000, u = rnorm(N))

  my_sampling <- declare_sampling(n = 100)

  my_assignment <- declare_assignment(m = 50)

  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = u,
                               Y_Z_1 = u + .25)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  #debugonce(DeclareDesign:::switching_equation)

  my_design <- declare_design(
    my_population,
    my_potential_outcomes,
    my_estimand,
    my_sampling,
    my_assignment,
    reveal_outcomes,
    my_estimator)

  dat <- draw_data(my_design)


  expect_true(
    all(vapply(my_design, function(step) "design_step" %in% class(step), FALSE)),
    "all steps should have appropriate class set"
  )


  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           reveal_outcomes)

  head(draw_data(design))


  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           declare_reveal(assignment_variables = Z,
                                           outcome_variables = Y))

  head(draw_data(design))

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           declare_reveal(assignment_variable = "Z",
                                           outcome_variable = "Y"))

  head(draw_data(design))

})

test_that("reveal multiple outcomes works", {

  N <- 25

  my_population <- declare_population(N = N, noise = rnorm(N))
  my_potential_outcomes1 <- declare_potential_outcomes(formula = Y1 ~ Z * .25, conditions = c(0, 1))
  my_potential_outcomes2 <- declare_potential_outcomes(formula = Y2 ~ Z * .5 + 3, conditions = c(0, 1))
  my_assignment <- declare_assignment(m = 10)

  design <- declare_design(my_population,
                           my_potential_outcomes1, my_potential_outcomes2,
                           my_assignment,
                           declare_reveal(outcome_variables = c(Y1, Y2)))
  draw_data(design)

  design <- declare_design(my_population,
                           my_potential_outcomes1, my_potential_outcomes2,
                           my_assignment,
                           declare_reveal(outcome_variables = c("Y1", "Y2")))
  draw_data(design)

})

test_that("declare_reveal handler works", {

  N <- 25

  my_population <- declare_population(N = N, noise = rnorm(N))
  my_assignment <- declare_assignment(m = 10)

  my_outcome_function <- function(data){
    data$Y <- rnorm(nrow(data))
    return(data)
  }

  design <- declare_design(my_population,
                           my_assignment,
                           declare_reveal(handler = my_outcome_function))
  draw_data(design)

})

test_that("missing PO stops",{

  expect_error(
    declare_reveal(outcome_variables = foo, assignment_variables = extra)(sleep)
  )
})

test_that("Not all Potential outcome columns present",{

  df <- data.frame(Z=sample(1:3, 100, replace=TRUE), Y_Z_0=1:100, Y_Z_1=1:100)

  expect_error(
    declare_reveal()(df),
    "Y_Z_3"
  )
})


test_that("Single outcome, multiple assn", {

  population <- declare_population(
    blocks = fabricatr::add_level(
      N = 40,
      block_shock = rnorm(N)),
    subjects  = fabricatr::add_level(
      N = 8,
      shock = rnorm(N) + block_shock)
  )

  # Model ----------------------------------------------------------------------
  potential_outcomes <- declare_potential_outcomes(
    Y_A_0_B_0 = 1,  Y_A_1_B_0 = 2,
    Y_A_0_B_1 = 3,  Y_A_1_B_1 = 4)


  # Factorial assignments
  assign_A <- declare_assignment(
    block_var = blocks, assignment_variable = A)
  assign_B <- declare_assignment(
    block_var = A + 10*as.numeric(blocks), assignment_variable = B)

  design <- declare_design(
    population,
    potential_outcomes,
    assign_A,   assign_B,
    declare_reveal(outcome_variables = Y, assignment_variables = c(A,B))
  )

  dd <- draw_data(design)

  expect_equal(c(table(dd$Y)), c(80,80,80,80), check.attributes=FALSE)

})
