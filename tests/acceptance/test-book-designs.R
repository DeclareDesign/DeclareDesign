# Designs from the DeclareDesign book that need modelling packages from
# outside DeclareDesign's dependencies.
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
# 12 of the book's 90 design declarations run here: 11 verbatim and 1 after
# the one mechanical substitution fabricatr requires. 72 more need nothing
# beyond DeclareDesign's own dependencies and run in
# tests/testthat/test-book-designs.R. The remaining 6 are listed at the foot
# of this file, with the reason for each.

# Every test here skips on CRAN. These designs need rdss, rdrobust, metafor,
# MatchIt, and other modelling packages that have no place in this package's
# dependencies. They run locally and in CI, where those packages are
# installed.
skip_unless <- function(...) {
  skip_on_cran()
  for (p in c(...)) skip_if_not_installed(p)
}

# choosing answer strategy ----

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

# redesigning ----

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

test_that("design runs (complex)", {
  skip_unless("randomizr", "estimatr", "rdss", "metafor")
  tau <- (c(0, 1))[[1]]
  mu <- 0.2 # true PATE
  tau <- 0.0 # true SD of site-level ATEs
  design <-
    declare_model(
      N = 200,
      site = 1:N,
      std.error = pmax(0.1, abs(rnorm(N, mean = 0.8, sd = 0.5))),
      theta = rnorm(N, mean = mu, sd = tau), # when tau = 0, theta = mu 
      estimate = rnorm(N, mean = theta, sd = std.error)
    ) + 
    declare_inquiry(mu = mu, tau_sq = tau^2) + 
    declare_estimator(
      yi = estimate, sei = std.error, method = "REML",
      .method = rma_helper, .summary = rma_mu_tau,
      term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
      label = "random-effects") + 
    declare_estimator(
      yi = estimate, sei = std.error, method = "FE",
      .method = rma_helper, .summary = rma_mu_tau,
      term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
      label = "fixed-effects")
  expect_s3_class(design, "design")
  estimates <- draw_estimates(design)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# experimental descriptive ----

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

test_that("declaration_16.5 runs (observational causal)", {
  skip_unless("randomizr", "estimatr", "rdss", "rdrobust")
  cutoff <- 0.5
  control <- function(X) {
    as.vector(poly(X - cutoff, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
  treatment <- function(X) {
    as.vector(poly(X - cutoff, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}
  declaration_16.5 <-
    declare_model(
      N = 500,
      U = rnorm(N, 0, 0.1),
      X = runif(N, 0, 1) + U,
      D = 1 * (X > cutoff),
      Y_D_0 = control(X) + U,
      Y_D_1 = treatment(X) + U
    ) +
    declare_inquiry(LATE = treatment(cutoff) - control(cutoff)) +
    declare_measurement(Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(
      Y, X, c = cutoff,
      term = "Bias-Corrected",
      .method = rdrobust_helper,
      inquiry = "LATE",
      label = "optimal"
    )
  expect_s3_class(declaration_16.5, "design")
  estimates <- draw_estimates(declaration_16.5)
  expect_gt(nrow(estimates), 0)
  if ("estimate" %in% names(estimates))
    expect_false(all(is.na(estimates$estimate)))
})

# observational descriptive ----

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

# Ported from fabricatr syntax ----
#
# These declarations use fabricatr's `nest = FALSE` and
# `cross_levels(by = join_using(...))`, which fabricatr replaces with
# `declare_level()` and `.by = c(...)`. Both old spellings now error with a
# message naming the replacement, so the port is mechanical and is applied
# here. They are the only book designs whose text has to change.

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
#   declaration_19.4   fails identically under DeclareDesign 1.1.1, so not a difference between them
#   declaration_18.13  needs the interference package, which is not on CRAN
