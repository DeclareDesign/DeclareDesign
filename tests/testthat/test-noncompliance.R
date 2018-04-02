context("Noncompliance")

test_that("Noncompliance", {

  skip_if_not_installed("AER")

  my_population <- declare_population(N = 100, noise = rnorm(N))

  POS_Y <- declare_potential_outcomes(Y_D_0 = noise, Y_D_1 = Y_D_0 + 2)
  POS_Z <- declare_potential_outcomes(
    D_Z_0 = rbinom(n = N, size = 1, prob = pnorm(noise - 1)),
    D_Z_1 = rbinom(n = N, size = 1, prob = pnorm(noise + 1)))

  my_assignment <- declare_assignment(m = 50)

  CACE <- declare_estimand(CACE = mean(Y_D_1[complier == 1] - Y_D_0[complier == 1]))
  ITT_d <- declare_estimand(ITT_d = mean(complier))

  cace_estimator <- function(data, alpha = 0.05){
    fit <- AER::ivreg(Y ~ D | Z, data = data)
    N <- nrow(data)
    k <- 2
    df <- N - k

    coef <- coef(fit)
    se <- sqrt(diag(vcov(fit)))

    p <- 2 * pt(abs(coef), df = df, lower.tail = FALSE)
    ci_lower <- coef - qt(1 - alpha / 2, df = df) * se
    ci_upper <- coef + qt(1 - alpha / 2, df = df) * se

    return_frame <-
      data.frame(
        variable_names = names(coef),
        est = coef,
        se = se,
        p = p,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      )
    return_frame[return_frame$variable_names == "D",]
  }

  cace_hat <- declare_estimator(handler=tidy_estimator(cace_estimator), estimand = CACE, label="CACE_hat")

  design <- declare_design(my_population,
                           POS_Y,
                           POS_Z,
                           dplyr::mutate(complier = as.numeric(D_Z_0 == 0 & D_Z_1 == 1)),
                           ITT_d,
                           CACE,
                           my_assignment,
                           declare_reveal(outcome_variables = "D", assignment_variables = "Z"),
                           declare_reveal(outcome_variables = "Y", assignment_variables = "D"),
                           cace_hat)

  df <- draw_data(design)
  expect_true("complier" %in% colnames(df))

  diag <- diagnose_design(design, sims = 2, bootstrap = FALSE)

  expect_equal(diag$diagnosands$mean_estimand, 2)
  expect_equal(diag$diagnosands$estimator_label, "CACE_hat") # ITT_d is not in output - not estimated

})
