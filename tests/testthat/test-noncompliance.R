context("Noncompliance")

test_that("Noncompliance", {
  skip_if_not_installed("AER")

  my_population <- declare_population(N = 100, noise = rnorm(N))

  POS_Y <- declare_potential_outcomes(Y_D_0 = noise, Y_D_1 = Y_D_0 + 2)
  POS_Z <- declare_potential_outcomes(
    D_Z_0 = rbinom(n = N, size = 1, prob = pnorm(noise - 1)),
    D_Z_1 = rbinom(n = N, size = 1, prob = pnorm(noise + 1))
  )

  my_assignment <- declare_assignment(Z = complete_ra(N, m = 50))

  CACE <- declare_inquiry(CACE = mean(Y_D_1[complier == 1] - Y_D_0[complier == 1]))
  ITT_d <- declare_inquiry(ITT_d = mean(complier))

  cace_estimator <- function(data, alpha = 0.05) {
    fit <- AER::ivreg(Y ~ D | Z, data = data)
    N <- nrow(data)
    k <- 2
    df <- N - k

    coef <- coef(fit)
    std.error <- sqrt(diag(vcov(fit)))

    p.value <- 2 * pt(abs(coef), df = df, lower.tail = FALSE)
    conf.low <- coef - qt(1 - alpha / 2, df = df) * std.error
    conf.high <- coef + qt(1 - alpha / 2, df = df) * std.error

    return_frame <-
      data.frame(
        variable_names = names(coef),
        estimate = coef,
        std.error = std.error,
        p.value = p.value,
        conf.low = conf.low,
        conf.high = conf.high
      )
    return_frame[return_frame$variable_names == "D", ]
  }

  cace_hat <- declare_estimator(handler = label_estimator(cace_estimator), inquiry = CACE, label = "CACE_hat")

  design <- my_population +
    POS_Y +
    POS_Z +
    declare_step(fabricate, complier = as.numeric(D_Z_0 == 0 & D_Z_1 == 1)) +
    ITT_d +
    CACE +
    my_assignment +
    declare_reveal(outcome_variables = "D", assignment_variables = "Z") +
    declare_reveal(outcome_variables = "Y", assignment_variables = "D") +
    cace_hat

  df <- draw_data(design)
  expect_true("complier" %in% colnames(df))

  diag <- diagnose_design(design, sims = 2, bootstrap_sims = FALSE)

  expect_equal(diag$diagnosands$mean_estimand[1], 2)
  expect_equal(diag$diagnosands$estimator[1], "CACE_hat") 
  # ITT_d is not in output - not estimated: AC: NOW IT IS!
})

test_that("POs correctly assembled for noncompliance case", {
  pop <- declare_population(
    N = 10000,
    type = sample(
      c("Complier", "Never-taker", "Always-taker"),
      size = N,
      prob = c(0.5, 0.2, 0.3),
      replace = TRUE
    ),
    noise = rnorm(N)
  )

  df <- pop()

  pos_D <- declare_potential_outcomes(D ~ as.numeric(type == "Always-taker" | type == "Complier" & Z == 1))


  expect_equal(colnames(pos_D(df)), c("ID", "type", "noise", "D_Z_0", "D_Z_1"))

  pos_Y <- declare_potential_outcomes(
    Y ~ 0.4 * D * (type == "Complier") - 0.2 * (type == "Never-taker") +
      0.5 * (type == "Always-taker") +
      noise,
    assignment_variables = "D"
  )

  assignment <- declare_assignment(Z = complete_ra(N, prob = 0.5))

    noncompliance <-
      pop +
      pos_D +
      assignment +
      declare_reveal(D, Z) +
      pos_Y +
      declare_reveal(Y, D)

  e <- (noncompliance[[4]])

  expect_true(inherits(e, "design_step"))
  expect_equal(attr(e, "step_type"), "reveal")
  expect_equal(attr(e, "step_meta")$assignment_variables, "Z")
  expect_equal(attr(e, "step_meta")$outcome_variables, "D")
})


test_that("POS don't erase Z", {
  pop <- declare_population(N = 10, Z = rbinom(N, size = 1, prob = .5))
  po <- declare_potential_outcomes(Y ~ Z)
  df <- pop()
  expect_equal(df$Z, po(df)$Z)
})
