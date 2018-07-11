context("Multiple estimators")

test_that("Two estimators, Two estimands (matched)", {
  des <-
    declare_population(sleep) +
    declare_estimand(
      CATE_1_5 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 1:5
    ) +
    declare_estimand(
      CATE_6_10 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 6:10
    ) +
    declare_step(fabricate, extra = extra + rnorm(N)) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 1:5,
      model = difference_in_means,
      estimand = "CATE_1_5",
      label = "DIM_1_5",
      term = group2
    ) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 6:10,
      model = difference_in_means,
      estimand = "CATE_6_10",
      label = "DIM_6_10",
      term = group2
    )

  diag <- diagnose_design(des, sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diag$diagnosands), 2)
})


test_that("Two estimators, Two estimands (crossed)", {
  des <-
    declare_population(sleep) +
    # Make a noisier outcome
    declare_potential_outcomes(extra1 ~ extra + 2 * (Z == 1) + rnorm(length(extra))) +

    declare_estimand(ATE = mean(extra1_Z_1) - mean(extra1_Z_0)) +
    declare_estimand(ATT = mean(extra1_Z_1) - mean(extra1_Z_0), subset = group == 2) +

    declare_assignment() +
    declare_reveal(outcome_variables = extra1, assignment_variables = Z) +

    declare_estimator(extra1 ~ Z, model = difference_in_means, estimand = c("ATE", "ATT"), label = "DIM") +
    declare_estimator(extra1 ~ Z + group, model = lm_robust, clusters = ID, estimand = c("ATE", "ATT"), label = "OLS + control")

  diag <- diagnose_design(des, sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diag$diagnosands), 4)
})
