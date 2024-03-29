context("Checking Code in Paper Works")

# “Characterizing Research Designs in Code" -------------------------------

test_that("section on 'Characterizing Research Designs in Code' works", {
  my_population <- function(N) {
    data.frame(u = rnorm(N))
  }

  population <-
    declare_model(handler = my_population, N = 500)

  my_sampling <- function(data) {
    data$S <- rbinom(
      n = nrow(data),
      size = 1,
      prob = 0.1
    )
    data <- data[data$S == 1, ]
    data$S <- NULL
    data
  }

  sampling <- declare_sampling(handler = my_sampling)

  my_assignment <- function(data) {
    data$Z <- rbinom(
      n = nrow(data),
      size = 1,
      prob = 0.5
    )
    data
  }

  assignment <-
    declare_assignment(handler = my_assignment)

  my_potential_outcomes <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }

  potential_outcomes <- declare_potential_outcomes(
    handler = my_potential_outcomes
  )

  my_inquiry <- function(data) {
    with(
      data,
      data.frame(inquiry = "inquiry", estimand = mean(Y_Z_1 - Y_Z_0), stringsAsFactors = FALSE)
    )
  }

  inquiry <- declare_inquiry(handler = my_inquiry)

  my_estimator <- function(data) {
    reg <- lm(Y ~ Z, data = data)
    phi <- t(data.frame(summary(reg)$coefficients["Z", ]))
    colnames(phi) <- c("estimate", "std.error", "t", "p.value")
    phi
  }

  estimator <- declare_estimator(handler = label_estimator(my_estimator), inquiry = inquiry)

  diagnosand <- declare_diagnosands(bias = mean(estimate - estimand), keep_defaults = FALSE)

  measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

  design <-
    population +
    sampling +
    assignment +
    potential_outcomes +
    inquiry +
    measurement +
    estimator

  df <- draw_data(design)

  expect_equal(colnames(df), c("u", "Z", "Y_Z_0", "Y_Z_1", "Y"))

  run_design(design)

  diagnose_design(
    design = design,
    diagnosands = diagnosand,
    sims = 2, bootstrap_sims = FALSE
  )
})
