context("Multiple estimands can be mapped to one estimator")


test_that("", {
  pop_var <- function(x) mean((x - mean(x))^2)
  x <- rnorm(100)
  dat <- data.frame(X = x)
  sx <- sum((dat$X - mean(dat$X))^2)
  simp_pop <- declare_population(
    epsilon = rnorm(N, sd = 2),
    Y = X + epsilon
  )

  dgp_se <- declare_estimand(dgp_se = 2 / sqrt(sx))
  obs_se <- declare_estimand(obs_se = sqrt(pop_var(epsilon) / sqrt(sx)))

  lmc <- declare_estimator(
    Y ~ X,
    model = estimatr::lm_robust,
    se_type = "classical",
    estimand = c(dgp_se, obs_se),
    coefficient_name = "X"
  )

  des <- declare_design(
    dat,
    simp_pop,
    dgp_se, obs_se,
    lmc
  )

  my_dig <- declare_diagnosands(
    bias_se = mean(se - estimand),
    bias_est = mean(se - sd(estimand)),
    mean_se = mean(se),
    sd_se = sd(se),
    mean_estimand = mean(estimand),
    sd_estimand = sd(estimand)
  )

  diag <- diagnose_design(des, sims = 2, diagnosands = my_dig, bootstrap = FALSE, parallel = FALSE)

  expect_equal(nrow(get_diagnosands(diag)), 2)
  expect_true(!any(is.na(get_diagnosands(diag)$bias_se)))
})
