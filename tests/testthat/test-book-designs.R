# Designs from the DeclareDesign book, run as a regression suite.
#
# Scraped from https://book.declaredesign.org on 2026-07-28 and reduced to the
# smallest self-contained form: each test carries the declaration verbatim plus
# only the earlier chapter code it refers to. The scraper and the assembler are
# in notes/probes/, so this file can be rebuilt when the book changes.
#
# Every design is declared and then executed, and an estimator that returns
# nothing but NA counts as a failure. A design that assembles and then dies on
# the first draw is not a passing example, and neither is one that quietly
# produces no estimate.
#
# 68 of 90 book declarations run here. The 22 that do not are listed at the
# bottom with the reason for each.

# The book leans on randomizr and estimatr throughout, and on rdss for its
# datasets and handlers. All are Suggests, so each test says what it needs.
skip_unless <- function(...) for (p in c(...)) skip_if_not_installed(p)

# choosing answer strategy ----

test_that("declaration_9.1 runs (choosing answer strategy)", {
  skip_unless("randomizr", "estimatr")
  declaration_9.1 <-
    declare_model(N = 100, age = sample(0:80, size = N, replace = TRUE)) +
    declare_inquiry(mean_age = mean(age)) +
    declare_sampling(S = complete_rs(N = N, n = 3)) +
    declare_estimator(age ~ 1, .method = lm_robust)
  expect_s3_class(declaration_9.1, "design")
  estimates <- draw_estimates(declaration_9.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_9.5 runs (choosing answer strategy)", {
  skip_unless("randomizr", "estimatr", "rdss")
  declaration_9.5 <-
    declare_model(data = resample_data(clingingsmith_etal)) +
    declare_estimator(views ~ success, .method = difference_in_means)
  expect_s3_class(declaration_9.5, "design")
  estimates <- draw_estimates(declaration_9.5)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("MI runs (choosing answer strategy)", {
  skip_unless("randomizr", "estimatr")
  MI <-
    declare_model(
      N = 100,
      X = rbinom(N, size = 1, 0.5),
      U = rnorm(N),
      potential_outcomes(Y ~ 0.5 * Z+-0.5 * X + 0.5 * X * Z + U)
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  expect_s3_class(MI, "design")
  estimates <- draw_estimates(MI)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# declaration in code ----

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M1 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M1 <- 
    declare_model(
      N = 1000, 
      Y = rbinom(N, 1, prob = 0.5)
    )
  expect_s3_class(M1, "design_step")
  expect_s3_class(M1(NULL), "data.frame")
})

test_that("M2 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M2 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1),
      Y = rbinom(N, 1, prob = latent),
      X = latent + rnorm(N)
    )
  expect_s3_class(M2, "design_step")
  expect_s3_class(M2(NULL), "data.frame")
})

test_that("M3 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M3 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1), 
      Y = if_else(latent > 0.75, 1, 0)
    )
  expect_s3_class(M3, "design_step")
  expect_s3_class(M3(NULL), "data.frame")
})

test_that("M1 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M1 <- 
    declare_model(
      N = 1000, 
      Y = rbinom(N, 1, prob = 0.5)
    )
  M1 <- 
    declare_model(
      N = 1000,
      X1 = rnorm(N),
      X2 = X1 + rnorm(N)
    )
  expect_s3_class(M1, "design_step")
  expect_s3_class(M1(NULL), "data.frame")
})

test_that("M2 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M2 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1),
      Y = rbinom(N, 1, prob = latent),
      X = latent + rnorm(N)
    )
  M2 <-
    declare_model(
      draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
        n = 1000,
        mu = c(0, 0),
        Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
      )))
  expect_s3_class(M2, "design_step")
  expect_s3_class(M2(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M1 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M1 <- 
    declare_model(
      N = 1000, 
      Y = rbinom(N, 1, prob = 0.5)
    )
  M1 <- 
    declare_model(
      N = 1000,
      X1 = rnorm(N),
      X2 = X1 + rnorm(N)
    )
  M1 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, size = 1, prob = 0.5),
      Y = 0.1 * X + U
    )
  expect_s3_class(M1, "design_step")
  expect_s3_class(M1(NULL), "data.frame")
})

test_that("M2 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M2 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1),
      Y = rbinom(N, 1, prob = latent),
      X = latent + rnorm(N)
    )
  M2 <-
    declare_model(
      draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
        n = 1000,
        mu = c(0, 0),
        Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
      )))
  M2 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, size = 1, prob = pnorm(U)),
      Y = 0.1 * X + U
    )
  expect_s3_class(M2, "design_step")
  expect_s3_class(M2(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M1 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M1 <- 
    declare_model(
      N = 1000, 
      Y = rbinom(N, 1, prob = 0.5)
    )
  M1 <- 
    declare_model(
      N = 1000,
      X1 = rnorm(N),
      X2 = X1 + rnorm(N)
    )
  M1 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, size = 1, prob = 0.5),
      Y = 0.1 * X + U
    )
  M1 <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, 1, prob = 0.2))
    )
  expect_s3_class(M1, "design_step")
  expect_s3_class(M1(NULL), "data.frame")
})

test_that("M2 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M2 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1),
      Y = rbinom(N, 1, prob = latent),
      X = latent + rnorm(N)
    )
  M2 <-
    declare_model(
      draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
        n = 1000,
        mu = c(0, 0),
        Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
      )))
  M2 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, size = 1, prob = pnorm(U)),
      Y = 0.1 * X + U
    )
  M2 <- 
    declare_model(
      N = 100,
      latent = rnorm(N), 
      potential_outcomes(Y ~ rbinom(N, 1, prob = pnorm(latent + 0.2 * Z)))
    )
  expect_s3_class(M2, "design_step")
  expect_s3_class(M2(NULL), "data.frame")
})

test_that("M3 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M3 <- 
    declare_model(
      N = 1000, 
      latent = runif(N, min = 0, max = 1), 
      Y = if_else(latent > 0.75, 1, 0)
    )
  M3 <- 
    declare_model(
      N = 100, 
      latent = rnorm(N), 
      potential_outcomes(Y ~ if_else(latent + 0.2 * Z > 0.5, 1, 0))
    )
  expect_s3_class(M3, "design_step")
  expect_s3_class(M3(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  M <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2 * X + 0.1 * Z * X + U)
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  M <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2 * X + 0.1 * Z * X + U)
    )
  M <- 
    declare_model(
      counties = add_level(N = 5, county_mean = rnorm(N)),
      individuals = add_level(N = 50, Y = rnorm(N, mean = county_mean))
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  M <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2 * X + 0.1 * Z * X + U)
    )
  M <- 
    declare_model(
      counties = add_level(N = 5, county_mean = rnorm(N)),
      individuals = add_level(N = 50, Y = rnorm(N, mean = county_mean))
    )
  M <-
    declare_model(N = 100,
                  X = rbinom(N, 1, prob = 0.5))
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  M <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2 * X + 0.1 * Z * X + U)
    )
  M <- 
    declare_model(
      counties = add_level(N = 5, county_mean = rnorm(N)),
      individuals = add_level(N = 50, Y = rnorm(N, mean = county_mean))
    )
  M <-
    declare_model(N = 100,
                  X = rbinom(N, 1, prob = 0.5))
  M <- declare_model(N = 100, latent = runif(N))
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("M runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = add_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10), 
        nest = FALSE
      ),
      observation = cross_levels(
        by = join_using(countries, years),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  M <- 
    declare_model(
      N = 100, 
      X = runif(N, min = 0, max = 100)
    )
  M <-
    declare_model(
      N = 1000,
      X1 = rnorm(N, mean = 5, sd = 2),
      X2 = runif(N, min = 0, max = 5),
      X3 = rbinom(N, size = 1, prob = 0.5),
      X4 = rbinom(N, size = 5, prob = 0.5),
      X5 = rlnorm(N, meanlog = 0, sdlog = 1),
      X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
    )
  M <-
    declare_model(households = add_level(N = 1000),
                  individuals = add_level(
                    N = 4,
                    X = draw_normal_icc(
                      mean = 0,
                      clusters = households,
                      ICC = 0.65
                    )
                  ))
  M <- 
    declare_model(
      data = baseline_data,
      attitudes = sample(1:5, N, replace = TRUE)
    )
  M <-
    declare_model(
      data = baseline_data, 
      N = 619505, 
      handler = resample_data
    )
  M <-
    declare_model(
      N = 100,
      Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
      Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
        conditions = list(Z = c(0, 1, 2))
      )
    )
  M <- 
    declare_model(
      N = 100, 
      potential_outcomes(
        Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
      )
    )
  M <-
    declare_model(
      N = 100, 
      tau = runif(1, min = 0, max = 1), 
      U = rnorm(N), 
      potential_outcomes(Y ~ tau * Z + U)
    )
  M <- 
    declare_model(
      N = 100, 
      U = rnorm(N), 
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
    )
  M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
  M <- declare_model(N = 100, Y = rnorm(N))
  M <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      potential_outcomes(Y ~  0.3 * Z + 0.2 * X + 0.1 * Z * X + U)
    )
  M <- 
    declare_model(
      counties = add_level(N = 5, county_mean = rnorm(N)),
      individuals = add_level(N = 50, Y = rnorm(N, mean = county_mean))
    )
  M <-
    declare_model(N = 100,
                  X = rbinom(N, 1, prob = 0.5))
  M <- declare_model(N = 100, latent = runif(N))
  M <-
    declare_model(
      N = 100,
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("declaration_13.1 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  declaration_13.1 <-
    declare_model(N = 100,
                  U = rnorm(N),
                  potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(
      Y ~ Z,
      .method = lm_robust,
      .summary = tidy,
      term = "Z",
      inquiry = "ATE",
      label = "OLS"
    )
  expect_s3_class(declaration_13.1, "design")
  estimates <- draw_estimates(declaration_13.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("model runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  model <-
    declare_model(N = 1000,
                  U = rnorm(N),
                  X = U + rnorm(N, sd = 0.5),
                  potential_outcomes(Y ~  0.2 * Z + U))
  expect_s3_class(model, "design_step")
  expect_s3_class(model(NULL), "data.frame")
})

test_that("declaration_13.2 runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  model <-
    declare_model(N = 1000,
                  U = rnorm(N),
                  X = U + rnorm(N, sd = 0.5),
                  potential_outcomes(Y ~  0.2 * Z + U))
  inquiry <-
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  sampling <-
    declare_sampling(S = simple_rs(N, prob = 0.2), 
                     filter = S == 1)
  assignment <-
    declare_assignment(Z = complete_ra(N))
  measurement <-
    declare_measurement(Y = reveal_outcomes(Y ~ Z))
  answer_strategy <-
    declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
    declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "OLS")
  declaration_13.2 <-
    model +
    inquiry +
    sampling +
    assignment +
    measurement +
    answer_strategy
  declaration_13.2 <-
    declare_model(N = 1000,
                  U = rnorm(N),
                  X = U + rnorm(N, sd = 0.5),
                  potential_outcomes(Y ~  0.2 * Z + U)) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(S = simple_rs(N, prob = 0.2),
                     filter = S == 1) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
    declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "OLS")
  expect_s3_class(declaration_13.2, "design")
  estimates <- draw_estimates(declaration_13.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("design runs (declaration in code)", {
  skip_unless("randomizr", "estimatr")
  design <-
    declare_model(
      N = 200,
      U = rnorm(N),
      potential_outcomes(Y ~ runif(1, 0.0, 0.5) * Z + U)
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(design, "design")
  estimates <- draw_estimates(design)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# declaring designs ----

test_that("declaration_5.1 runs (declaring designs)", {
  skip_unless("randomizr", "estimatr")
  declaration_5.1 <-
    declare_model(
      N = 1000,
      U = rnorm(N),
      X = rbinom(N, 1, prob = pnorm(U)),
      Y = rbinom(N, 1, prob = pnorm(U + X))
    ) +
    declare_inquiry(Ybar = mean(Y[X == 1])) +
    declare_sampling(S = simple_rs(N, prob = 0.1)) +
    declare_estimator(Y ~ 1,
                      .method = lm_robust,
                      subset = X == 1,
                      inquiry = "Ybar")
  expect_s3_class(declaration_5.1, "design")
  estimates <- draw_estimates(declaration_5.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# defining inquiry ----

test_that("declaration_7.1 runs (defining inquiry)", {
  skip_unless("randomizr", "estimatr")
  declaration_7.1 <-
    declare_model(
      N = 20, 
      U = rnorm(N),
      Y = 1 + U
    ) +
    declare_inquiry(
      superpopulation_mean = 1,
      population_mean = mean(Y)
    ) + 
    declare_sampling(
      S = complete_rs(N, n = 10)
    ) +
    declare_inquiry(sample_mean = mean(Y))
  expect_s3_class(declaration_7.1, "design")
  estimates <- draw_estimates(declaration_7.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# design example ----

test_that("model_12.1 runs (design example)", {
  skip_unless("randomizr", "estimatr")
  model_12.1 <- 
    declare_model(
      villages = add_level(N = 660, U_village = rnorm(N, sd = 0.1)),
      citizens = add_level(
        N = 100,
        U_citizen = rnorm(N),
        potential_outcomes(
          Y ~ pnorm(
            U_citizen + U_village +
              0.10 * (Z == "personal") +
              0.15 * (Z == "social")),
          conditions = list(Z = c("neutral", "personal", "social"))
        )
      )
    )
  expect_s3_class(model_12.1, "design_step")
  expect_s3_class(model_12.1(NULL), "data.frame")
})

# diagnosing designs ----

test_that("declaration_10.1 runs (diagnosing designs)", {
  skip_unless("randomizr", "estimatr")
  declaration_10.1 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      potential_outcomes(Y ~  0.2 * Z + U)
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(declaration_10.1, "design")
  estimates <- draw_estimates(declaration_10.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_10.2 runs (diagnosing designs)", {
  skip_unless("randomizr", "estimatr")
  declaration_10.2 <-
    declare_model(
      N = 200, U = rnorm(N),
      # this runif(n = 1, min = 0, max = 0.5) 
      # generates 1 random ATE between 0 and 0.5
      potential_outcomes(Y ~ runif(n = 1, min = 0, max = 0.5) * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(declaration_10.2, "design")
  estimates <- draw_estimates(declaration_10.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("M1 runs (diagnosing designs)", {
  skip_unless("randomizr", "estimatr")
  M1 <-
    declare_model(
      N = 200,
      U = rnorm(N),
      potential_outcomes(Y1 ~ 0.2 * Z + U),
      potential_outcomes(Y2 ~ 0.0 * Z + U)
    )
  expect_s3_class(M1, "design_step")
  expect_s3_class(M1(NULL), "data.frame")
})

test_that("M2 runs (diagnosing designs)", {
  skip_unless("randomizr", "estimatr")
  M2 <-
    declare_model(
      N = 200,
      U = rnorm(N),
      potential_outcomes(Y1 ~ 0.0 * Z + U),
      potential_outcomes(Y2 ~ 0.2 * Z + U)
    )
  expect_s3_class(M2, "design_step")
  expect_s3_class(M2(NULL), "data.frame")
})

# redesigning ----

test_that("declaration_11.1 runs (redesigning)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  declaration_11.1 <-
    declare_model(N = N) +
    declare_measurement(Y = rbinom(n = N, size = 1, prob = 0.55)) +
    declare_test(handler =
                   label_estimator(function(data) {
                     test <- prop.test(x = table(data$Y), p = 0.5)
                     tidy(test)
                   }))
  expect_s3_class(declaration_11.1, "design")
  estimates <- draw_estimates(declaration_11.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_11.2 runs (redesigning)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  N <- 100
  declaration_11.2 <-
    declare_model(N = N, U = rnorm(N),
                  # this runif(n = 1, min = 0, max = 0.5) generates 1 random ATE between 0 and 0.5
                  potential_outcomes(Y ~ runif(n = 1, min = 0, max = 0.5) * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(declaration_11.2, "design")
  estimates <- draw_estimates(declaration_11.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_11.4 runs (redesigning)", {
  skip_unless("randomizr", "estimatr", "stringr")
  N <- 100
  N <- 100
  N <- 100
  dip <- function(x) (x <= 1) * x + (x > 1) * (x - 2) ^ 2 + 0.2
  x_range <- seq(from = 0, to = 3, length.out = 50)
  polynomial_degrees <- 1:6
  declaration_11.4 <-
    declare_model(
      N = 100,
      X = runif(N, 0, 3)) +
    declare_inquiry(
      X = x_range, inquiry = str_c("X_", X), estimand = dip(X),
      data = NULL, handler = tibble
    ) +
    declare_measurement(Y = dip(X) + rnorm(N, 0, .5)) +
    declare_estimator(handler = function(data) {
      map(polynomial_degrees, ~lm(Y ~ poly(X, .), data = data)) |> 
        set_names(nm = str_c("A", polynomial_degrees)) |> 
        map_dfc(~predict(., newdata = tibble(X = x_range))) |> 
        bind_cols(tibble(X = x_range)) |> 
        mutate(inquiry = str_c("X_", X)) |> 
        pivot_longer(cols = starts_with("A"),
                     names_to = "estimator",
                     values_to = "estimate")
    })
  expect_s3_class(declaration_11.4, "design")
  estimates <- draw_estimates(declaration_11.4)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# what is a research design ----

test_that("model runs (what is a research design)", {
  skip_unless("randomizr", "estimatr")
  b <- 0
  model <- 
    declare_model(
      N = 1000,
      history = sample(c(0, 1), N, replace = TRUE),
      potential_outcomes(Y ~ b * history + runif(1, 0, 0.5) * Z + rnorm(N)))
  expect_s3_class(model, "design_step")
  expect_s3_class(model(NULL), "data.frame")
})

# experimental causal ----

test_that("declaration_18.1 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  declaration_18.1 <-
    declare_model(N = 100,
                  U = rnorm(N),
                  potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(declaration_18.1, "design")
  estimates <- draw_estimates(declaration_18.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.2 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  r_sq <- 0
  declaration_18.2 <-
    declare_model(N = N,
                  draw_multivariate(c(U, X) ~ MASS::mvrnorm(
                    n = N,
                    mu = c(0, 0),
                    Sigma = matrix(c(1, sqrt(r_sq), sqrt(r_sq), 1), 2, 2)
                  )), 
                  potential_outcomes(Y ~ 0.1 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(
      Y ~ Z, covariates = ~X, .method = lm_lin, inquiry = "ATE"
    )
  expect_s3_class(declaration_18.2, "design")
  estimates <- draw_estimates(declaration_18.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.4 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  declaration_18.4 <-
    declare_model(
      N = 500,
      X = rep(c(0, 1), each = N / 2),
      U = rnorm(N, sd = 0.25),
      potential_outcomes(Y ~ 0.2 * Z + X + U)
    ) +
    declare_assignment(
      Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
      probs =
        obtain_condition_probabilities(assignment = Z, 
                                       blocks = X, 
                                       block_prob = c(0.2, 0.5)),
      ipw = 1 / probs
    ) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(
      Y ~ Z,
      covariates = ~ X,
      .method = lm_lin,
      weights = ipw,
      label = "Lin"
    )
  expect_s3_class(declaration_18.4, "design")
  estimates <- draw_estimates(declaration_18.4)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.5 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  ICC <- 0.9
  declaration_18.5 <-
    declare_model(
      cluster =
        add_level(
          N = 10,
          cluster_size = rep(seq(10, 50, 10), 2),
          cluster_shock =
            scale(cluster_size + rnorm(N, sd = 5)) * sqrt(ICC),
          cluster_tau = rnorm(N, sd = sqrt(ICC))
        ),
      individual =
        add_level(
          N = cluster_size,
          individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
          individual_tau = rnorm(N, sd = sqrt(1 - ICC)),
          Y_Z_0 = cluster_shock + individual_shock,
          Y_Z_1 = Y_Z_0 + cluster_tau + individual_tau
        )
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = block_and_cluster_ra(clusters = cluster, blocks = cluster_size)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z,
                      clusters = cluster,
                      inquiry = "ATE")
  expect_s3_class(declaration_18.5, "design")
  estimates <- draw_estimates(declaration_18.5)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.6 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  fixed_pop <-
    fabricate(N = 10000,
              X = rbinom(N, 1, 0.2),
              potential_outcomes(
                Y ~ rbinom(N, 1,
                           prob = 0.7 + 0.1 * Z  - 0.4 * X - 0.2 * Z * X))
    )
  total_n <- 1000
  n_x1 <- 500
  declaration_18.6 <-
    declare_model(data = fixed_pop) +
    declare_inquiry(
      CATE_X1 = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
      CATE_X0 = mean(Y_Z_1[X == 0] - Y_Z_0[X == 0]),
      diff_in_CATEs = CATE_X1 - CATE_X0
    ) +
    declare_sampling(
      S = strata_rs(strata = X, strata_n = c(total_n - n_x1, n_x1))
    ) +
    declare_assignment(Z = block_ra(blocks = X)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z + X + Z * X, 
                      term = "Z:X", 
                      inquiry = "diff_in_CATEs")
  expect_s3_class(declaration_18.6, "design")
  estimates <- draw_estimates(declaration_18.6)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.7 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  CATE_Z1_Z2_0 <- 0.2
  CATE_Z2_Z1_0 <- 0.1
  interaction <- 0.1
  N <- 1000
  declaration_18.7 <-
    declare_model(
      N = N,
      U = rnorm(N),
      potential_outcomes(Y ~ CATE_Z1_Z2_0 * Z1 +
                           CATE_Z2_Z1_0 * Z2 +
                           interaction * Z1 * Z2 + U,
                         conditions = list(Z1 = c(0, 1),
                                           Z2 = c(0, 1)))) +
    declare_inquiry(
      CATE_Z1_Z2_0 = mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0),
      CATE_Z1_Z2_1 = mean(Y_Z1_1_Z2_1 - Y_Z1_0_Z2_1),
      ATE_Z1 = 0.5 * CATE_Z1_Z2_0 + 0.5 * CATE_Z1_Z2_1,
      
      CATE_Z2_Z1_0 = mean(Y_Z1_0_Z2_1 - Y_Z1_0_Z2_0),
      CATE_Z2_Z1_1 = mean(Y_Z1_1_Z2_1 - Y_Z1_1_Z2_0),
      ATE_Z2 = 0.5 * CATE_Z2_Z1_0 + 0.5 * CATE_Z2_Z1_1,
      
      diff_in_CATEs_Z1 = CATE_Z1_Z2_1 - CATE_Z1_Z2_0,
      #equivalently
      diff_in_CATEs_Z2 = CATE_Z2_Z1_1 - CATE_Z2_Z1_0
    ) + 
    declare_assignment(Z1 = complete_ra(N),
                       Z2 = block_ra(Z1)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z1 + Z2)) +
    declare_estimator(Y ~ Z1, subset = (Z2 == 0), 
                      inquiry = "CATE_Z1_Z2_0", label = "1") +
    declare_estimator(Y ~ Z1, subset = (Z2 == 1), 
                      inquiry = "CATE_Z1_Z2_1", label = '2') +
    declare_estimator(Y ~ Z2, subset = (Z1 == 0), 
                      inquiry = "CATE_Z2_Z1_0", label = "3") +
    declare_estimator(Y ~ Z2, subset = (Z1 == 1),
                      inquiry = "CATE_Z2_Z1_1", label = "4") +
    declare_estimator(Y ~ Z1 + Z2, term = c("Z1", "Z2"), 
                      inquiry = c("ATE_Z1", "ATE_Z2"), label = "5") +
    declare_estimator(Y ~ Z1 + Z2 + Z1*Z2, term = "Z1:Z2", 
                      inquiry = c("diff_in_CATEs_Z1", "diff_in_CATEs_Z2"), 
                      label = "6")
  expect_s3_class(declaration_18.7, "design")
  estimates <- draw_estimates(declaration_18.7)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.8 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  N <- 1000
  declaration_18.8 <-
    declare_model(
      N = 100,
      type = 
        rep(c("Always-Taker", "Never-Taker", "Complier", "Defier"),
            c(0.2, 0.2, 0.6, 0.0)*N),
      U = rnorm(N),
      # potential outcomes of Y with respect to D
      potential_outcomes(
        Y ~ case_when(
          type == "Always-Taker" ~ -0.25 - 0.50 * D + U,
          type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
          type == "Complier" ~ 0.25 + 0.50 * D + U,
          type == "Defier" ~ -0.25 - 0.50 * D + U
        ),
        conditions = list(D = c(0, 1))
      ),
      # potential outcomes of D with respect to Z
      potential_outcomes(
        D ~ case_when(
          Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
          Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
          Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
          Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
        ),
        conditions = list(Z = c(0, 1))
      )
    ) +
    declare_inquiry(
      ATE = mean(Y_D_1 - Y_D_0),
      CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"])) +
    declare_assignment(Z = conduct_ra(N = N)) +
    declare_measurement(D = reveal_outcomes(D ~ Z),
                        Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(
      Y ~ D | Z,
      .method = iv_robust,
      inquiry = c("ATE", "CACE"),
      label = "Two stage least squares"
    ) +
    declare_estimator(
      Y ~ D,
      .method = lm_robust,
      inquiry = c("ATE", "CACE"),
      label = "As treated"
    ) +
    declare_estimator(
      Y ~ D,
      .method = lm_robust,
      inquiry = c("ATE", "CACE"),
      subset = D == Z,
      label = "Per protocol"
    )
  expect_s3_class(declaration_18.8, "design")
  estimates <- draw_estimates(declaration_18.8)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("MI runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  N <- 1000
  compliance_rate <- 0.2
  MI <-
    declare_model(
      N = 400,
      type = sample(x = c("Never-Taker", "Complier"), 
                    size = N,
                    prob = c(1 - compliance_rate, compliance_rate),
                    replace = TRUE),
      U = rnorm(N),
      # potential outcomes of Y with respect to D
      potential_outcomes(
        Y ~ case_when(
          type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
          type == "Complier" ~ 0.25 + 0.50 * D + U
        ),
        conditions = list(D = c(0, 1))
      ),
      # potential outcomes of D with respect to Z
      potential_outcomes(
        D ~ if_else(Z == 1 & type == "Complier", 1, 0),
        conditions = list(Z = c(0, 1))
      )
    ) +
    declare_inquiry(
      CACE = mean(Y_D_1[type == "Complier"] - 
                    Y_D_0[type == "Complier"])
    )
  expect_s3_class(MI, "design")
  estimates <- draw_estimates(MI)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.12 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  N <- 1000
  declaration_18.12 <-
    declare_model(
      group = add_level(N = 50, group_shock = rnorm(N)),
      individual = add_level(
        N = 20,
        individual_shock = rnorm(N),
        potential_outcomes(
          Y ~ 0.2 * Z + 0.1 * (S == "low") + 0.5 * (S == "high") +
            group_shock + individual_shock,
          conditions = list(Z = c(0, 1),
                            S = c("low", "high"))
        )
      )
    ) +
    declare_inquiry(
      CATE_S_Z_0 = mean(Y_Z_0_S_high - Y_Z_0_S_low),
      CATE_Z_S_low = mean(Y_Z_1_S_low - Y_Z_0_S_low)
    ) +
    declare_assignment(
      S = cluster_ra(clusters = group, 
                     conditions = c("low", "high")),
      Z = block_ra(blocks = group, 
                   prob_unit = if_else(S == "low", 0.25, 0.75))
    ) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z + S)) +
    declare_estimator(
      Y ~ S,
      .method = difference_in_means,
      subset = Z == 0,
      term = "Shigh",
      clusters = group,
      inquiry = "CATE_S_Z_0",
      label = "Effect of high saturation among untreated"
    ) +
    declare_estimator(
      Y ~ Z,
      .method = difference_in_means,
      subset = S == "low",
      blocks = group,
      inquiry = "CATE_Z_S_low",
      label = "Effect of treatment at low saturation"
    )
  expect_s3_class(declaration_18.12, "design")
  estimates <- draw_estimates(declaration_18.12)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# experimental descriptive ----

test_that("declaration_17.1 runs (experimental descriptive)", {
  skip_unless("randomizr", "estimatr")
  declaration_17.1 <-
    declare_model(
      N = 500,
      type = sample(
        size = N, 
        replace = TRUE,
        x = c("Always-responder",
              "Anti-Latino discriminator",
              "Never-responder"),
        prob = c(0.30, 0.05, 0.65)
      ),
      # Behavioral assumptions represented here:
      Y_Z_white = if_else(type == "Never-Responder", 0, 1),
      Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
    ) +
    declare_inquiry(
      anti_latino_discrimination = mean(type == "Anti-Latino discriminator")
    ) +
    declare_assignment(Z = complete_ra(N, conditions = c("latino", "white"))) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "anti_latino_discrimination")
  expect_s3_class(declaration_17.1, "design")
  estimates <- draw_estimates(declaration_17.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_17.2 runs (experimental descriptive)", {
  skip_unless("randomizr", "estimatr")
  declaration_17.2 <-
    # This part of the design is about causal inference
    declare_model(
      N = 5000,
      type_D_0 = sample(
        size = N,
        replace = TRUE,
        x = c("Always-Responder",
              "Anti-Latino Discriminator",
              "Never-Responder"),
        prob = c(0.30, 0.05, 0.65)
      ),
      type_tau_i = rbinom(N, 1, 0.5),
      type_D_1 = if_else(
        type_D_0 == "Anti-Latino Discriminator" &
          type_tau_i == 1,
        "Always-Responder",
        type_D_0
      )
    ) +
    declare_inquiry(
      ATE = mean((type_D_1 == "Anti-Latino Discriminator") -
                   (type_D_0 == "Anti-Latino Discriminator"))
    ) +
    declare_assignment(D = complete_ra(N)) +
    declare_measurement(type = reveal_outcomes(type ~ D)) +
    # This part is about descriptive inference in each condition!
    declare_model(
      Y_Z_white = if_else(type == "Never-Responder", 0, 1),
      Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
    ) +
    declare_assignment(
      Z = complete_ra(N, conditions = c("latino", "white"))) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z * D, term = "Zwhite:D", inquiry = "ATE")
  expect_s3_class(declaration_17.2, "design")
  estimates <- draw_estimates(declaration_17.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_17.3 runs (experimental descriptive)", {
  skip_unless("randomizr", "estimatr")
  declaration_17.3 <-
    declare_model(
      N = 500,
      control_count = rbinom(N, size = 3, prob = 0.5),
      Y_star = rbinom(N, size = 1, prob = 0.3),
      potential_outcomes(Y_list ~ Y_star * Z + control_count) 
    ) +
    declare_inquiry(prevalence_rate = mean(Y_star)) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
    declare_estimator(Y_list ~ Z, .method = difference_in_means, 
                      inquiry = "prevalence_rate")
  expect_s3_class(declaration_17.3, "design")
  estimates <- draw_estimates(declaration_17.3)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# observational causal ----

test_that("declaration_16.4 runs (observational causal)", {
  skip_unless("randomizr", "estimatr")
  declaration_16.4 <-
    declare_model(
      N = 100, 
      U = rnorm(N),
      potential_outcomes(D ~ if_else(Z + U > 0, 1, 0), 
                         conditions = list(Z = c(0, 1))), 
      potential_outcomes(Y ~ 0.1 * D + 0.25 + U, 
                         conditions = list(D = c(0, 1))),
      complier = D_Z_1 == 1 & D_Z_0 == 0
    ) + 
    declare_inquiry(LATE = mean(Y_D_1 - Y_D_0), subset = complier == TRUE) + 
    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(D = reveal_outcomes(D ~ Z),
                        Y = reveal_outcomes(Y ~ D)) + 
    declare_estimator(Y ~ D | Z, .method = iv_robust, inquiry = "LATE")
  expect_s3_class(declaration_16.4, "design")
  estimates <- draw_estimates(declaration_16.4)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# observational descriptive ----

test_that("declaration_15.1 runs (observational descriptive)", {
  skip_unless("randomizr", "estimatr")
  portola <-
    fabricate(
      N = 2100,
      Y_star = rnorm(N)
    )
  declaration_15.1 <-
    declare_model(data = portola) +
    declare_measurement(Y = as.numeric(cut(Y_star, 7))) +
    declare_inquiry(Y_bar = mean(Y)) +
    declare_sampling(S = complete_rs(N, n = 100)) +
    declare_estimator(Y ~ 1, inquiry = "Y_bar")
  expect_s3_class(declaration_15.1, "design")
  estimates <- draw_estimates(declaration_15.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_15.2 runs (observational descriptive)", {
  skip_unless("randomizr", "estimatr")
  portola <-
    fabricate(
      N = 2100,
      Y_star = rnorm(N)
    )
  effort <- 0
  declaration_15.2 <- 
    declare_model(data = portola) + 
    declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
    declare_inquiry(Y_bar = mean(Y)) + 
    declare_sampling(S = complete_rs(N, n = 100)) + 
    declare_measurement(
      R = rbinom(n = N, size = 1, prob = pnorm(Y_star + effort)),
      Y = if_else(R == 1, Y, NA_real_)
    ) +
    declare_estimator(Y ~ 1, inquiry = "Y_bar") +
    declare_estimator(R ~ 1, label = "Response Rate")
  expect_s3_class(declaration_15.2, "design")
  estimates <- draw_estimates(declaration_15.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_15.6 runs (observational descriptive)", {
  skip_unless("randomizr", "estimatr")
  declaration_15.6 <-
    declare_model(
      N = 500,
      X = rep(0:1, N / 2),
      Y_star = 1 + X + 2 * rnorm(N)
    ) +
    declare_inquiry(Y_bar_X1 = mean(scale(Y_star)[X == 1])) +
    declare_measurement(
      Y_1 = 3 + 0.1 * Y_star + rnorm(N, sd = 5),
      Y_2 = 2 + 1.0 * Y_star + rnorm(N, sd = 2),
      Y_3 = 1 + 0.5 * Y_star + rnorm(N, sd = 1),
      Y_avg = ((scale(Y_1) + scale(Y_2) + scale(Y_3)))/3,
      Y_avg_adjusted = (
        # rescaling according to the X = 0 group
        (Y_1 - mean(Y_1[X == 0])) / sd(Y_1[X == 0]) +
          (Y_2 - mean(Y_2[X == 0])) / sd(Y_2[X == 0]) +
          (Y_3 - mean(Y_3[X == 0])) / sd(Y_3[X == 0])
      ) / 3,
      Y_avg_rescaled = scale((scale(Y_1) + scale(Y_2) + scale(Y_3))),
      Y_first_factor  = princomp( ~ Y_1 + Y_2 + Y_2, cor = TRUE)$scores[, 1]
    ) +
    declare_estimator(
      cbind(Y_avg, Y_avg_adjusted, Y_avg_rescaled, Y_first_factor) ~ 1,
      .method = lm_robust,
      inquiry = "Y_bar_X1",
      subset = X == 1,
      term = TRUE,
      label = "Average"
    )
  expect_s3_class(declaration_15.6, "design")
  estimates <- draw_estimates(declaration_15.6)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# integration ----

test_that("model_1 runs (integration)", {
  skip_unless("randomizr", "estimatr")
  model_1 <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rnorm(N),
      Z = rbinom(N, 1, prob = plogis(0.5)),
      potential_outcomes(Y ~ 0.1 * Z + 0.25 * X + U),
      Y = reveal_outcomes(Y ~ Z)
    )
  expect_s3_class(model_1, "design_step")
  expect_s3_class(model_1(NULL), "data.frame")
})

test_that("model_2 runs (integration)", {
  skip_unless("randomizr", "estimatr")
  model_2 <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rnorm(N),
      Z = rbinom(N, 1, prob = plogis(0.5 + X)),
      potential_outcomes(Y ~ 0.1 * Z + 0.25 * X + U),
      Y = reveal_outcomes(Y ~ Z)
    )
  expect_s3_class(model_2, "design_step")
  expect_s3_class(model_2(NULL), "data.frame")
})

test_that("model_3 runs (integration)", {
  skip_unless("randomizr", "estimatr")
  model_3 <- 
    declare_model(
      N = 100,
      U = rnorm(N),
      Z = rbinom(N, 1, prob = plogis(0.5)),
      potential_outcomes(Y ~ 0.1 * Z + U),
      Y = reveal_outcomes(Y ~ Z),
      X = 0.1 * Z + 5 * Y + rnorm(N)
    )
  expect_s3_class(model_3, "design_step")
  expect_s3_class(model_3(NULL), "data.frame")
})

# Ported from fabricatr syntax ----
#
# These declarations use fabricatr's `nest = FALSE` and
# `cross_levels(by = join_using(...))`, which fabricatrZero replaces with
# `declare_level()` and `.by = c(...)`. Both old spellings now error with a
# message naming the replacement, so the port is mechanical and is applied
# here. They are the only book designs whose text has to change.

test_that("M runs (declaration in code), ported", {
  skip_unless("randomizr", "estimatr")
  M <- declare_model(N = 1000)
  M <- 
    declare_model(
      households = add_level(
        N = 100, 
        N_members = sample(c(1, 2, 3, 4), N, 
                           prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
      ),
      individuals = add_level(
        N = N_members, 
        age = sample(18:90, N, replace = TRUE)
      )
    )
  M <- 
    declare_model(
      countries = add_level(
        N = 196, 
        country_shock = rnorm(N)
      ),
      years = declare_level(
        N = 100, 
        time_trend = 1:N,
        year_shock = runif(N, 1, 10)
      ),
      observation = cross_levels(
        .by = c("countries", "years"),
        observation_shock = rnorm(N),
        Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
      )
    )
  expect_s3_class(M, "design_step")
  expect_s3_class(M(NULL), "data.frame")
})

test_that("declaration_18.10 runs (experimental causal), ported", {
  skip_unless("randomizr", "estimatr")
  N <- 100
  N <- 1000
  effect_size <- 0.35
  declaration_18.10 <-
    declare_model(
      units = add_level(
        N = 100, 
        U_unit = rnorm(N)
      ),
      periods = declare_level(
        N = 3,
        time = 1:max(periods),
        U_time = rnorm(N)
      ),
      unit_period = cross_levels(
        .by = c("units", "periods"),
        U = rnorm(N),
        potential_outcomes(
          Y ~ scale(U_unit + U_time + time + U) + effect_size * Z
        )
      )
    ) +
    declare_assignment(
      wave = cluster_ra(clusters = units, conditions = 1:max(periods)),
      Z = if_else(time >= wave, 1, 0)
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0), subset = time < max(time)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, fixed_effects = ~ periods + units, 
                      clusters = units, 
                      subset = time < max(time),
                      inquiry = "ATE", label = "TWFE")
  expect_s3_class(declaration_18.10, "design")
  estimates <- draw_estimates(declaration_18.10)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_16.3 runs (observational causal), ported", {
  skip_unless("randomizr", "estimatr", "DIDmultiplegt", "rdss")
  N_units <- 20
  N_time_periods <- 20
  declaration_16.3 <-
    declare_model(
    units = add_level(
      N = N_units,
      U_unit = rnorm(N),
      D_unit = if_else(U_unit > median(U_unit), 1, 0),
      D_time = sample(1:N_time_periods, N, replace = TRUE)
    ),
    periods = declare_level(
      N = N_time_periods,
      U_time = rnorm(N)
    ),
    unit_period = cross_levels(
      .by = c("units", "periods"),
      U = rnorm(N),
      potential_outcomes(Y ~ U + U_unit + U_time +
                           D * (0.2 - 1 * (D_time - as.numeric(periods))),
                         conditions = list(D = c(0, 1))),
      D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0),
      D_lag = lag_by_group(D, groups = units, n = 1, order_by = periods)
    )
  ) +
    declare_inquiry(
      ATT = mean(Y_D_1 - Y_D_0),
      subset = D == 1
    ) +
    declare_inquiry(
      ATT_switchers = mean(Y_D_1 - Y_D_0),
      subset = D == 1 & D_lag == 0 & !is.na(D_lag)
    ) +
    declare_measurement(Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(
      Y ~ D, fixed_effects = ~ units + periods,
      .method = lm_robust,
      inquiry = c("ATT", "ATT_switchers"),
      label = "twoway-fe"
    ) +
    declare_estimator(
      Y = "Y",
      G = "units",
      T = "periods",
      D = "D",
      mode = "old",
      handler = label_estimator(did_multiplegt_tidy),
      inquiry = c("ATT", "ATT_switchers"),
      label = "chaisemartin"
    )
  expect_s3_class(declaration_16.3, "design")
  estimates <- draw_estimates(declaration_16.3)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# Not covered ----
#
# Every book declaration this suite does not run, and why.
#
#   declaration_9.3    needs the rstanarm package
#   base_declaration   the book sets true_mean in prose rather than in a code chunk
#   declaration_9.7    the book sets block_m in prose rather than in a code chunk
#   M                  the book sets baseline_data in prose rather than in a code chunk
#   M                  the book sets baseline_data in prose rather than in a code chunk
#   declaration_11.3   the book sets prob in prose rather than in a code chunk
#   declaration_11.5   needs the margins package
#   declaration_19.1   the book sets X.1 in prose rather than in a code chunk
#   declaration_19.2   needs the bbmle package
#   design             open defect: pre-evaluated estimator dots defeat the method's own NSE
#   declaration_19.4   see notes/probes: ℹ In argument: `tidy(lm_robust(Y ~ Z_implemented))`. ℹ In gr
#   declaration_18.3   the book sets control_slope in prose rather than in a code chunk
#   declaration_18.11  the book sets n_units in prose rather than in a code chunk
#   declaration_18.13  needs the spdep package
#   declaration_17.4   the book sets N in prose rather than in a code chunk
#   declaration_17.5   needs the cjoint package
#   declaration_17.6   open defect: pre-evaluated dots defeat a tidyselect handler
#   declaration_16.1   needs the CausalQueries package
#   declaration_16.2   needs the MatchIt package
#   declaration_16.5   open defect: pre-evaluated dots defeat a tidyselect handler
#   declaration_15.3   the book sets cluster_prob in prose rather than in a code chunk
#   declaration_15.4   needs the lme4 package
