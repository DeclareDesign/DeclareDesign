context("Multiple inquiries can be mapped to one estimator")


test_that("Multiple inquiries can be mapped to one estimator", {
  pop_var <- function(x) {
    mean((x - mean(x))^2)
  }

  x <- rnorm(100)
  dat <- data.frame(X = x)
  sx <- sum((dat$X - mean(dat$X))^2)



  simp_pop <- declare_model(
    epsilon = rnorm(N, sd = 2),
    Y = X + epsilon
  )

  dgp_se <- declare_inquiry(dgp_se = 2 / sqrt(sx))
  obs_se <- declare_inquiry(obs_se = sqrt(pop_var(epsilon) / sqrt(sx)))

  lmc <- declare_estimator(
    Y ~ X,
    .method = estimatr::lm_robust,
    se_type = "classical",
    inquiry = c(dgp_se, obs_se),
    term = "X"
  )

  des <-
    declare_model(dat) +
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
    sd_inquiry = sd(estimand)
  )

  diag <- diagnose_design(des, sims = 2, diagnosands = my_dig, bootstrap_sims = FALSE)

  expect_equal(nrow(get_diagnosands(diag)), 2)
  expect_true(!any(is.na(get_diagnosands(diag)$bias_se)))
})


test_that("More multiple inquiries", {
  my_smp_fun <- function(data) {
    S <- rbinom(n = nrow(data), size = 1, prob = pnorm(data$noise))
    return(data[S == 1, , drop = FALSE])
  }


  pop <- declare_model(N = 100, noise = rnorm(N))
  pos <- declare_potential_outcomes(Y ~ Z * noise)
  pate <- declare_inquiry(pate = mean(Y_Z_1 - Y_Z_0))
  smp <- declare_sampling(handler = my_smp_fun)
  sate <- declare_inquiry(sate = mean(Y_Z_1 - Y_Z_0))
  assgn <- declare_assignment(Z = complete_ra(N, m = 10))
  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
  mator_both <- declare_estimator(Y ~ Z, inquiry = c(pate, sate))



  des <- pop + pos + pate + smp + sate + assgn + my_measurement + mator_both
  expect_equal(draw_estimates(des)$inquiry, c("pate", "sate"))
})

