context("Multiple estimands can be mapped to one estimator")


test_that("Multiple estimands can be mapped to one estimator", {
  pop_var <- function(x) {
    mean((x - mean(x))^2)
  }

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
    term = "X"
  )

  des <-
    declare_population(dat) +
    simp_pop +
    dgp_se +
    obs_se +
    lmc

  my_dig <- declare_diagnosands(
    bias_se = mean(std.error - estimand),
    bias_est = mean(std.error - sd(estimand)),
    mean_se = mean(std.error),
    sd_se = sd(std.error),
    mean_estimand = mean(estimand),
    sd_estimand = sd(estimand)
  )

  diag <- diagnose_design(des, sims = 2, diagnosands = my_dig, bootstrap_sims = FALSE)

  expect_equal(nrow(get_diagnosands(diag)), 2)
  expect_true(!any(is.na(get_diagnosands(diag)$bias_se)))
})


test_that("More multiple estimands", {
  my_smp_fun <- function(data) {
    S <- rbinom(n = nrow(data), size = 1, prob = pnorm(data$noise))
    return(data[S == 1, , drop = FALSE])
  }


  pop <- declare_population(N = 100, noise = rnorm(N))
  pos <- declare_potential_outcomes(Y ~ Z * noise)
  pate <- declare_estimand(pate = mean(Y_Z_1 - Y_Z_0))
  smp <- declare_sampling(handler = my_smp_fun)
  sate <- declare_estimand(sate = mean(Y_Z_1 - Y_Z_0))
  assgn <- declare_assignment(m = 10)
  my_reveal <- declare_reveal()
  mator_both <- declare_estimator(Y ~ Z, estimand = c(pate, sate))



  des <- pop + pos + pate + smp + sate + assgn + my_reveal + mator_both
  expect_equal(get_estimates(des)$estimand_label, c("pate", "sate"))
})
