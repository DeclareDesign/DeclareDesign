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
                           reveal_outcomes(assignment_variable_names = Z,
                                           outcome_variable_names = Y))

  head(draw_data(design))

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_assignment,
                           reveal_outcomes(assignment_variable_name = "Z",
                                           outcome_variable_name = "Y"))

  head(draw_data(design))

})

test_that("reveal multiple outcomes works", {

  N <- 25

  my_population <- declare_population(N = N, noise = rnorm(N))
  my_potential_outcomes1 <- declare_potential_outcomes(formula = Y1 ~ Z * .25, condition_names = c(0, 1))
  my_potential_outcomes2 <- declare_potential_outcomes(formula = Y2 ~ Z * .5 + 3, condition_names = c(0, 1))
  my_assignment <- declare_assignment(m = 10)

  design <- declare_design(my_population,
                           my_potential_outcomes1, my_potential_outcomes2,
                           my_assignment,
                           reveal_outcomes(outcome_variable_name = c(Y1, Y2)))
  draw_data(design)

  design <- declare_design(my_population,
                           my_potential_outcomes1, my_potential_outcomes2,
                           my_assignment,
                           reveal_outcomes(outcome_variable_name = c("Y1", "Y2")))
  draw_data(design)

})

test_that("outcome functions works", {

  N <- 25

  my_population <- declare_population(N = N, noise = rnorm(N))
  my_assignment <- declare_assignment(m = 10)

  my_outcome_function <- function(data){
    data$Y <- rnorm(nrow(data))
    return(data)
  }

  design <- declare_design(my_population,
                           my_assignment,
                           reveal_outcomes(outcome_function = my_outcome_function))
  draw_data(design)

})




