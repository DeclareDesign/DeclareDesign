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
# 81 of the book's 90 design declarations run here: 78 verbatim and 3 after
# the one mechanical substitution fabricatrZero requires. The remaining
# 9 are listed at the foot of the file, with the reason for each.

# Every test here skips on CRAN. The suite is large, and between them these
# designs need a dozen modelling packages that have no place in this
# package's dependencies. They run locally and in CI, where those packages
# are installed.
skip_unless <- function(...) {
  skip_on_cran()
  for (p in c(...)) skip_if_not_installed(p)
}

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

test_that("base_declaration runs (choosing answer strategy)", {
  skip_unless("randomizr", "estimatr")
  true_mean <- (seq(0, 100, length.out = 10))[[1]]
  base_declaration <-
    declare_model(N = 100, 
                  age = round(rnorm(N, mean = true_mean, sd = 23))) +
    declare_inquiry(mean_age = mean(age)) +
    declare_sampling(S = complete_rs(N = N, n = 3)) +
    declare_estimator(age ~ 1, .method = lm_robust)
  expect_s3_class(base_declaration, "design")
  estimates <- draw_estimates(base_declaration)
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

test_that("declaration_9.7 runs (choosing answer strategy)", {
  skip_unless("randomizr", "estimatr", "rdss")
  block_m = c(71, 47, 60, 48, 35, 39, 63, 32, 52)
  declaration_9.7 <-
    declare_model(data = foos_etal,
                  # this is the sharp null hypothesis
                  potential_outcomes(Y ~ 0 * Z + marked_register_2014)) +
    declare_assignment(Z = block_and_cluster_ra(blocks = ward, 
                                                clusters = street, 
                                                block_m = block_m),
                       probs = obtain_condition_probabilities(
                         assignment = Z,
                         blocks = ward,
                         clusters = street,
                         block_m = block_m
                       ),
                       ipw = 1 / probs) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z + ward, weights = ipw, clusters = street)
  expect_s3_class(declaration_9.7, "design")
  estimates <- draw_estimates(declaration_9.7)
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (seq(100, 1000, 100))[[1]]
  N <- (c(100, 500, 1000))[[1]]
  N <- (seq(100, 1000, 25))[[1]]
  N <- (seq(10, 100, by = 10))[[1]]
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
  N <- (seq(100, 1000, 100))[[1]]
  N <- (c(100, 500, 1000))[[1]]
  N <- (seq(100, 1000, 25))[[1]]
  N <- (seq(10, 100, by = 10))[[1]]
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

test_that("declaration_11.3 runs (redesigning)", {
  skip_unless("randomizr", "estimatr")
  N <- (seq(100, 1000, 100))[[1]]
  N <- (c(100, 500, 1000))[[1]]
  N <- (seq(100, 1000, 25))[[1]]
  prob <- (seq(0.1, 0.5, 0.2))[[1]]
  N <- (seq(10, 100, by = 10))[[1]]
  N <- 100
  N <- 100
  N <- 100
  declaration_11.3 <-
    declare_model(N = N, U = rnorm(N),
                  potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N = N, prob = prob)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  expect_s3_class(declaration_11.3, "design")
  estimates <- draw_estimates(declaration_11.3)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_11.4 runs (redesigning)", {
  skip_unless("randomizr", "estimatr", "stringr")
  N <- (seq(100, 1000, 100))[[1]]
  N <- (c(100, 500, 1000))[[1]]
  N <- (seq(100, 1000, 25))[[1]]
  N <- (seq(10, 100, by = 10))[[1]]
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

test_that("declaration_11.5 runs (redesigning)", {
  skip_unless("randomizr", "estimatr", "margins")
  N <- (seq(100, 1000, 100))[[1]]
  N <- (c(100, 500, 1000))[[1]]
  N <- (seq(100, 1000, 25))[[1]]
  N <- (seq(10, 100, by = 10))[[1]]
  N <- 100
  N <- 100
  N <- 100
  tidy_margins <- function(x) {
    tidy(margins(x, data = x$data), conf.int = TRUE)
  }
  N <- 10
  declaration_11.5 <-
    declare_model(N = N,
                  U = rnorm(N),
                  potential_outcomes(Y ~ rbinom(N, 1, prob = 0.2 * Z + 0.6))) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z,
                      inquiry = "ATE",
                      term = "Z",
                      label = "OLS") +
    declare_estimator(
      Y ~ Z,
      .method = glm,
      family = binomial("logit"),
      .summary = tidy_margins,
      inquiry = "ATE",
      term = "Z",
      label = "logit"
    ) +
    declare_estimator(
      Y ~ Z,
      .method = glm,
      family = binomial("probit"),
      .summary = tidy_margins,
      inquiry = "ATE",
      term = "Z",
      label = "probit"
    )
  expect_s3_class(declaration_11.5, "design")
  estimates <- draw_estimates(declaration_11.5)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# what is a research design ----

test_that("model runs (what is a research design)", {
  skip_unless("randomizr", "estimatr")
  b <- (seq(0, 3, 0.25))[[1]]
  b <- 0
  model <- 
    declare_model(
      N = 1000,
      history = sample(c(0, 1), N, replace = TRUE),
      potential_outcomes(Y ~ b * history + runif(1, 0, 0.5) * Z + rnorm(N)))
  expect_s3_class(model, "design_step")
  expect_s3_class(model(NULL), "data.frame")
})

# complex ----

test_that("declaration_19.2 runs (complex)", {
  skip_unless("randomizr", "estimatr", "bbmle")
  offer <- function(n, d){
    sum(sapply(2:n[1], function(t) ((-1)^t)*(d^{t-1})))
  }
  likelihood  <- function(n){
    function(k, d, a) {
      m <- Z * offer(n, d) + (1 - Z) * (1 - offer(n, d))
      R <- a * dbeta(y, k * .75, k * .25) + 
        (1 - a) * dbeta(y, k * m, k * (1 - m))
      return(-sum(log(R)))
    }
  }
  n <- 2
  delta <- 0.8
  kappa <- 2
  alpha <- 0.5
  declaration_19.2 <- 
    declare_model(
      # Define the population: indicator for behavioral type (norm = 1)
      N = 200, 
      type = rbinom(N, 1, alpha),
      n = n) +
    declare_inquiry(kappa = kappa,     
                    delta = delta,     
                    alpha = alpha) +   
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(
      # Equilibrium payoff
      pi = type * .75 + 
        (1 - type) * (Z * offer(n, delta) + (1 - Z) * (1 -offer(n, delta))), 
      # Actual payoff (stochastic)
      y = rbeta(N, pi * kappa, (1 - pi) * kappa))+
    # Estimation via maximum likelihood
    declare_estimator(.method = mle2,
                      minuslogl = likelihood(n),
                      start = list(k = 2, d = 0.50, a = 0.50),
                      lower = list(k = 0.10, d = 0.01, a = 0.01),
                      upper = list(k = 100, d = 0.99, a = 0.99),
                      method = "L-BFGS-B",
                      term = c("k", "d", "a"),
                      inquiry = c("kappa","delta", "alpha"), 
                      label = "Structural model")
  expect_s3_class(declaration_19.2, "design")
  estimates <- draw_estimates(declaration_19.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# experimental causal ----

test_that("declaration_18.1 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- (seq(500, 3000, 500))[[1]]
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
  r_sq <- (seq(0, 0.9, by = 0.2))[[1]]
  N <- (seq(500, 3000, 500))[[1]]
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

test_that("declaration_18.3 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  control_slope <- (seq(-1, 1, 0.5))[[1]]
  prob <- (seq(0.1, 0.9, 0.1))[[1]]
  N <- (seq(500, 3000, 500))[[1]]
  N <- 100
  prob = 0.5
  control_slope = -1
  declaration_18.3 <-
    declare_model(N = 100,
                  X = runif(N, 0, 1),
                  U = rnorm(N, sd = 0.1),
                  Y_Z_1 = 1*X + U,
                  Y_Z_0 = control_slope*X + U
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N = N, prob = prob)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
    declare_estimator(Y ~ Z + X, .method = lm_robust, inquiry = "ATE", label = "OLS") +
    declare_estimator(Y ~ Z, covariates = ~X, .method = lm_lin, inquiry = "ATE", label = "Lin")
  expect_s3_class(declaration_18.3, "design")
  estimates <- draw_estimates(declaration_18.3)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.4 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- (seq(500, 3000, 500))[[1]]
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
  ICC <- (seq(0.1, 0.9, by = 0.4))[[1]]
  N <- (seq(500, 3000, 500))[[1]]
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
  n_x1 <- (seq(20, 980, by = 96))[[1]]
  N <- (seq(500, 3000, 500))[[1]]
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
  N <- (seq(500, 3000, 500))[[1]]
  CATE_Z1_Z2_0 <- (seq(0, 0.5, 0.05))[[1]]
  CATE_Z2_Z1_0 <- (0.2)[[1]]
  interaction <- (0)[[1]]
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
  N <- (seq(500, 3000, 500))[[1]]
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
  N <- (seq(500, 3000, 500))[[1]]
  compliance_rate <- (seq(0.1, 0.9, by = 0.1))[[1]]
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

test_that("declaration_18.11 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- (seq(500, 3000, 500))[[1]]
  n_units <- (100)[[1]]
  effect_size <- (seq(from = 0, to = 0.75, by = 0.05))[[1]]
  n_units <- (200)[[1]]
  N <- 100
  N <- 1000
  effect_size <- 0.35
  declaration_18.11 <-
    declare_model(
      N = n_units, 
      U_unit = rnorm(N),
      U = rnorm(N),
      effect_size = effect_size,
      potential_outcomes(Y ~ scale(U_unit + U) + effect_size * Z)
    ) +
    declare_assignment(Z = complete_ra(N, m = n_units / 2)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM")
  expect_s3_class(declaration_18.11, "design")
  estimates <- draw_estimates(declaration_18.11)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_18.12 runs (experimental causal)", {
  skip_unless("randomizr", "estimatr")
  N <- (seq(500, 3000, 500))[[1]]
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
  N <- (seq(from = 500, to = 2500, by = 500))[[1]]
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
  N <- (seq(from = 500, to = 2500, by = 500))[[1]]
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
  N <- (seq(from = 500, to = 2500, by = 500))[[1]]
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

test_that("declaration_17.4 runs (experimental descriptive)", {
  skip_unless("randomizr", "estimatr")
  proportion_hiding <- (seq(from = 0, to = 0.3, by = 0.1))[[1]]
  N <- (seq(from = 500, to = 2500, by = 500))[[1]]
  declaration_17.4 <- 
    declare_model(
      N = N,
      U = rnorm(N),
      control_count = rbinom(N, size = 3, prob = 0.5),
      Y_star = rbinom(N, size = 1, prob = 0.3),
      W = case_when(Y_star == 0 ~ 0L,
                    Y_star == 1 ~ rbinom(N, size = 1, prob = proportion_hiding)),
      potential_outcomes(Y_list ~ Y_star * Z + control_count)
    ) +
    declare_inquiry(prevalence_rate = mean(Y_star)) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z),
                        Y_direct = Y_star - W) +
    declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate", label = "list") + 
    declare_estimator(Y_direct ~ 1, inquiry = "prevalence_rate", label = "direct")
  expect_s3_class(declaration_17.4, "design")
  estimates <- draw_estimates(declaration_17.4)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_17.5 runs (experimental descriptive)", {
  skip_unless("randomizr", "estimatr", "cjoint", "rdss")
  N <- (seq(from = 500, to = 2500, by = 500))[[1]]
  N_subjects <- 500
  N_tasks <- 3
  levels_list =
    list(
      gender = c("Man", "Woman"),
      party = c("Left", "Right"),
      region = c("North", "South", "East", "West")
    )
  conjoint_utility <-
    function(data){
      data |>
        mutate(U = 0.25*(gender == "Woman")*(region %in% c("North", "East")) +
                 0.5*(party == "Right")*(region %in% c("North", "South")) + uij)
    }
  declaration_17.5 <-
    declare_model(
      subject = add_level(N = N_subjects),
      task = add_level(N = N_tasks, task = 1:N_tasks),
      profile = add_level(
        N = 2,
        profile = 1:2,
        uij = rnorm(N, sd = 1)
      )
    ) +
    declare_inquiry(handler = conjoint_inquiries,
                    levels_list = levels_list,
                    utility_fn = conjoint_utility) +
    declare_assignment(handler = conjoint_assignment,
                       levels_list = levels_list) +
    declare_measurement(handler = conjoint_measurement,
                        utility_fn = conjoint_utility) +
    declare_estimator(choice ~ gender + party + region,
                      respondent.id = "subject",
                      .method = amce)
  expect_s3_class(declaration_17.5, "design")
  estimates <- draw_estimates(declaration_17.5)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# observational causal ----

test_that("declaration_16.1 runs (observational causal)", {
  skip_unless("randomizr", "estimatr", "CausalQueries")
  causal_model <- make_model("X -> M -> Y <- W -> M") |>
    set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") |>
    set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")
  strategies = c("X-Y", "X-Y-M", "X-Y-W",  "X-Y-W-M")
  declaration_16.1 <-
    declare_model(draw_causal_type(causal_model)) +
    declare_inquiry(
      CoE =  query_distribution(
        causal_model, 
        query = "Y[X=1] - Y[X=0]", 
        parameters = causal_type)) +
    declare_measurement(
      handler = function(data)
        causal_model |>
        make_data(parameters = data$causal_type))  +
    declare_estimator(
      handler = label_estimator(process_tracing_estimator), 
      causal_model = causal_model,
      query = "Y[X=1] - Y[X=0]",
      strategies = strategies)
  expect_s3_class(declaration_16.1, "design")
  estimates <- draw_estimates(declaration_16.1)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_16.2 runs (observational causal)", {
  skip_unless("randomizr", "estimatr", "MatchIt")
  exact_matching <-
    function(data) {
      matched <- matchit(D ~ X, method = "exact", data = data)
      match.data(matched)
    }
  declaration_16.2 <-
    declare_model(
      N = 100,
      U = rnorm(N),
      X = rbinom(N, 1, prob = 0.5),
      D = rbinom(N, 1, prob = 0.25 + 0.5 * X),
      Y_D_0 = 0.2 * X + U,
      Y_D_1 = Y_D_0 + 0.5
    ) +
    declare_inquiry(ATT = mean(Y_D_1[D == 1] - Y_D_0[D == 1])) +
    declare_step(handler = exact_matching) +
    declare_measurement(Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(Y ~ D,
                      weights = weights,
                      .method = difference_in_means,
                      inquiry = "ATT",
                      label = "Matched difference-in-means") +
    declare_estimator(Y ~ D,
                      .method = difference_in_means,
                      inquiry = "ATT",
                      label = "Raw difference-in-means")
  expect_s3_class(declaration_16.2, "design")
  estimates <- draw_estimates(declaration_16.2)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

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
  effort <- (seq(0, 5, by = 0.5))[[1]]
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

test_that("declaration_15.3 runs (observational descriptive)", {
  skip_unless("randomizr", "estimatr")
  cluster_prob <- (seq(0.1, 0.9, 0.1))[[1]]
  ICC <- 0.4
  two_nigerian_states <-
    fabricate(
      state = add_level(N = 2, 
                        state_name = c("taraba", "kwara"),
                        state_mean = c(-0.2, 0.2)),
      locality = add_level(
        N = 500,
        locality_shock = rnorm(N, state_mean, sqrt(ICC))
      ),
      individual = add_level(
        N = 100,
        individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
        Y_star = locality_shock + individual_shock
      )
    )
  budget_function <- 
    function(cluster_prob){
      budget = 20000
      cluster_cost = 20
      individual_cost = 2
      n_clusters = 1000
      n_individuals_per_cluster = 100
      
      total_cluster_cost <-
        cluster_prob * n_clusters * cluster_cost
      
      remaining_funds <- budget - total_cluster_cost
      
      sampleable_individuals <- 
        cluster_prob * n_clusters * n_individuals_per_cluster
      
      individual_prob = 
        (remaining_funds/individual_cost)/sampleable_individuals
      
      pmin(individual_prob, 1)
    }
  declaration_15.3 <-
    declare_model(data = two_nigerian_states) +
    declare_measurement(Y = as.numeric(cut(Y_star, 7))) +
    declare_inquiry(Y_bar = mean(Y)) +
    declare_sampling(
      S_cluster = strata_and_cluster_rs(
        strata = state,
        clusters = locality,
        prob = cluster_prob
      ),
      filter = S_cluster == 1
    ) +
    declare_sampling(
      S_individual = 
        strata_rs(strata = locality, 
                  prob = budget_function(cluster_prob)),
      filter = S_individual == 1
    ) +
    declare_estimator(Y ~ 1,
                      clusters = locality,
                      se_type = "stata",
                      inquiry = "Y_bar")
  expect_s3_class(declaration_15.3, "design")
  estimates <- draw_estimates(declaration_15.3)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

test_that("declaration_15.4 runs (observational descriptive)", {
  skip_unless("randomizr", "estimatr", "rdss")
  states <- 
    as_tibble(state.x77) |>
    transmute(
      state = rownames(state.x77),
      prop_of_US = Population / sum(Population),
      # results in exactly 2,000 due to rounding
      state_n = round(prop_of_US * 1998.6), 
      prob_HS = `HS Grad` / 100,
      state_shock = rnorm(n = n(), sd = 0.5),
      state_mean = prob_HS * pnorm(0.2 + state_shock) + (1 - prob_HS) * pnorm(state_shock)
    )
  declaration_15.4 <-
    declare_model(
      data = states[rep(1:50, states$state_n), ],
      HS = rbinom(n = N, size = 1, prob = prob_HS),
      PS_weight =
        case_when(HS == 0 ~ (1 - prob_HS),
                  HS == 1 ~ prob_HS),
      individual_shock = rnorm(n = N, sd = 0.5),
      policy_support = 
        rbinom(N, 1, prob = pnorm(0.2 * HS + individual_shock + state_shock))
    ) +
    declare_inquiry(
      handler = function(data) {
        states |> transmute(
          state, 
          inquiry = "mean_policy_support", 
          estimand = state_mean
        )
      }
    ) +
    declare_estimator(handler = label_estimator(function(data) {
      model_fit <- glmer(
        formula = policy_support ~ HS + (1 | state),
        data = data,
        family = binomial(link = "logit")
      )
      post_stratification_helper(model_fit, data = data, group = state, weights = PS_weight)
    }),
    label = "Partial pooling",
    inquiry = "mean_policy_support")
  expect_s3_class(declaration_15.4, "design")
  estimates <- draw_estimates(declaration_15.4)
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
  N <- (c(100, 200, 300))[[1]]
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
  N <- (seq(500, 3000, 500))[[1]]
  effect_size <- (seq(from = 0, to = 0.75, by = 0.05))[[1]]
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
#   declaration_9.3    see notes/probes: could not find function "summary_fn"
#   M                  the book defines baseline_data outside any code chunk, so it cannot be reconstructed
#   M                  the book defines baseline_data outside any code chunk, so it cannot be reconstructed
#   declaration_19.1   the book defines X.1 outside any code chunk, so it cannot be reconstructed
#   design             OPEN DEFECT: pre-evaluated estimator dots defeat the method's own NSE
#   declaration_19.4   fails identically under DeclareDesign 1.1.1, so not a difference between them
#   declaration_18.13  needs the interference package, which is not on CRAN
#   declaration_17.6   OPEN DEFECT: pre-evaluated dots defeat a tidyselect handler
#   declaration_16.5   OPEN DEFECT: pre-evaluated dots defeat a tidyselect handler
