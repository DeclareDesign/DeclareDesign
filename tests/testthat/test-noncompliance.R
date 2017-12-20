context("Noncompliance")

test_that("Noncompliance", {

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

  cace_hat <- declare_estimator(estimator_function = cace_estimator, estimand = CACE)

  design <- declare_design(my_population,
                           POS_Y,
                           POS_Z,
                           dplyr::mutate(complier = as.numeric(D_Z_0 == 0 & D_Z_1 == 1)),
                           ITT_d,
                           CACE,
                           my_assignment,
                           reveal_outcomes(outcome_variable_name = "D", assignment_variable_name = "Z"),
                           reveal_outcomes(outcome_variable_name = "Y", assignment_variable_name = "D"),
                           cace_hat)


  head(draw_data(design))

  execute_design(design)

  df <- draw_data(design)
  cace_estimator(df)

  diagnose_design(design, sims = 2, bootstrap = FALSE, parallel = FALSE)


})
