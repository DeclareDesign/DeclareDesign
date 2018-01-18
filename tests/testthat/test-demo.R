context("Demo")

test_that("demo runs", {

  ## ------------------------------------------------------------------------
  my_population <-
    declare_population(N = 1000,
                       income = rnorm(N),
                       age = sample(18:95, N, replace = T))

  pop <- my_population()
  head(pop)

  ## ------------------------------------------------------------------------
  my_population_nested <- declare_population(
    districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages =add_level(N = 10, altitude = rnorm(N)),
    individuals = add_level(N = sample(100:200, size = 250, replace = TRUE),
                        income = rnorm(N),
                        age = sample(18:95, N, replace = TRUE)))


  ## ------------------------------------------------------------------------
  region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
  pop_level_data <- declare_population(
    regions = add_level(N = 2, gdp = runif(N)),
    cities = add_level(N = 2, subways = rnorm(N, mean = 5)))

  head(pop_level_data())

  ## ------------------------------------------------------------------------
  country_data <- data.frame(
    cow_code = c(504, 15, 100, 90),
    polity_iv = c(-9, 7, -1, 3))
  pop_data <- declare_population(data = country_data)

  head(pop_data())

  ## ------------------------------------------------------------------------
  pop_data_bootstrap <- declare_population(
    data = country_data, handler = fabricatr::resample_data)

  head(pop_data_bootstrap())

  ## ------------------------------------------------------------------------
  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z)
  pop_pos <- my_potential_outcomes(pop)
  head(pop_pos)

  ## ------------------------------------------------------------------------
  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z,
    condition_names = 1:4)
  head(my_potential_outcomes(pop))

  ## ------------------------------------------------------------------------
  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = .05,
      Y_Z_1 = .30 + .01 * age)

  head(my_potential_outcomes(pop))

  ## ------------------------------------------------------------------------
  my_sampling <- declare_sampling(n = 250)
  smp <- my_sampling(pop_pos)
  nrow(smp)

  ## ------------------------------------------------------------------------
  my_assignment <- declare_assignment(m = 25)
  smp <- my_assignment(smp)
  table(smp$Z)
  head(smp)

  ## ------------------------------------------------------------------------
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  my_estimand(pop_pos)

  ## ------------------------------------------------------------------------
  smp <- reveal_outcomes(smp)
  my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
  my_estimator_dim(smp)

  ## ------------------------------------------------------------------------
  my_estimator_lm <-
    declare_estimator(Y ~ Z,
                      model = estimatr::lm_robust,
                      coefficient_name = "Z",
                      estimand = my_estimand)

  my_estimator_lm(smp)

  ## ------------------------------------------------------------------------
  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_estimand,
                           dplyr::mutate(big_income = 5*income), #whoa!
                           my_sampling,
                           my_assignment,
                           reveal_outcomes,
                           my_estimator_dim)

  ## ------------------------------------------------------------------------
  dat <- draw_data(design)
  head(dat)

  ## ------------------------------------------------------------------------
  get_estimates(design)
  get_estimands(design)

  ## ------------------------------------------------------------------------
  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler = my_population_function, N = 100)

  pop_custom <- my_population_custom()

  head(pop_custom)

  ## ------------------------------------------------------------------------
  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }
  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler = my_potential_outcomes_function
  )

  pop_pos_custom <- my_potential_outcomes_custom(pop_custom)

  head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")])

  ## ------------------------------------------------------------------------
  my_sampling_function <- function(data) {
    data$S <- rbinom(n = nrow(data),
                     size = 1,
                     prob = 0.1)
    data[data$S == 1, ]
  }

  my_sampling_custom <- declare_sampling(
    handler = my_sampling_function)

  smp_custom <- my_sampling_custom(pop_pos)

  nrow(smp_custom)

  ## ------------------------------------------------------------------------
  my_assignment_function <- function(data) {
    data$Z <- rbinom(n = nrow(data),
                     size = 1,
                     prob = 0.5)
    data
  }
  my_assignment_custom <- declare_assignment(
    handler = my_assignment_function)

  table(my_assignment_custom(pop_pos)$Z)

  ## ------------------------------------------------------------------------
  my_estimand_function <- function(data) {
    with(data, median(Y_Z_1 - Y_Z_0))
  }
  my_estimand_custom <- declare_estimand(
    handler = my_estimand_function, label = "medianTE")

  my_estimand_custom(pop_pos)

  ## ------------------------------------------------------------------------
  my_estimator_function <- function(formula, data){
    data.frame(est = with(data, mean(Y)))
  }

  my_estimator_custom <-
    declare_estimator(Y ~ Z,
                      estimator_function = my_estimator_function,
                      estimand = my_estimand)

  my_estimator_custom(smp)

  ## ------------------------------------------------------------------------
  m_arm_trial <- function(numb){
    my_population <- declare_population(
      N = numb, income = rnorm(N), age = sample(18:95, N, replace = T))

    my_potential_outcomes <- declare_potential_outcomes(
      formula = Y ~ .25 * Z + .01 * age * Z)
    my_sampling <- declare_sampling(n = 250)
    my_assignment <- declare_assignment(m = 25)
    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                my_estimand,
                                my_sampling,
                                my_assignment,
                                reveal_outcomes,
                                my_estimator_dim)
    return(my_design)
  }

  my_1000_design <- quick_design(template = m_arm_trial, numb = 1000)
  head(draw_data(my_1000_design))

  ## ------------------------------------------------------------------------
  my_potential_outcomes_continuous <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z, condition_names = seq(0, 1, by = .1))

  continuous_treatment_function <- function(data){
    data$Z <- sample(seq(0, 1, by = .1), size = nrow(data), replace = TRUE)
    data
  }

  my_assignment_continuous <- declare_assignment(handler = continuous_treatment_function)

  my_design <- declare_design(my_population(),
                              my_potential_outcomes_continuous,
                              my_assignment_continuous,
                              reveal_outcomes)

  head(draw_data(my_design))

  ## ------------------------------------------------------------------------
  my_potential_outcomes_attrition <- declare_potential_outcomes(
    formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))

  my_design <- declare_design(my_population(),
                              my_potential_outcomes,
                              my_potential_outcomes_attrition,
                              my_assignment,
                              reveal_outcomes(outcome_variable_name = "R"),
                              reveal_outcomes(attrition_variable_name = "R"))

  head(draw_data(my_design)[, c("ID", "Y_Z_0", "Y_Z_1", "R_Z_0", "R_Z_1", "Z", "R", "Y")])

  ## ------------------------------------------------------------------------
  stochastic_population <- declare_population(
    N = sample(500:1000, 1), income = rnorm(N), age = sample(18:95, N, replace = TRUE))

  c(nrow(stochastic_population()),
    nrow(stochastic_population()),
    nrow(stochastic_population()))

  ## ------------------------------------------------------------------------

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  ## set a variable used in the declaration
  my_N <- 1000
  my_population_custom <- declare_population(
    handler = my_population_function, N = my_N)

  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }
  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler = my_potential_outcomes_function
  )

  ## remove all objects except your pop and PO functions
  rm(list = ls()[-which(ls() %in% c("my_potential_outcomes_custom", "my_population_custom"))])

  pop_pos_custom <- my_potential_outcomes_custom(my_population_custom())

  head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")])

})
