context("Multiple estimators")

test_that("Two estimators, Two inquiries (matched)", {
  des <-
    declare_model(sleep) +
    declare_inquiry(
      CATE_1_5 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 1:5
    ) +
    declare_inquiry(
      CATE_6_10 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 6:10
    ) +
    declare_step(fabricate, extra = extra + rnorm(N)) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 1:5,
      .method = difference_in_means,
      inquiry = "CATE_1_5",
      label = "DIM_1_5",
      term = "group2"
    ) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 6:10,
      .method = difference_in_means,
      inquiry = "CATE_6_10",
      label = "DIM_6_10",
      term = "group2"
    )

  diag <- diagnose_design(des, sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diag$diagnosands), 2)
})


test_that("Two estimators, Two inquiries (crossed)", {
  des <-
    declare_model(sleep) +
    # Make a noisier outcome
    declare_potential_outcomes(extra1 ~ extra + 2 * (Z == 1) + rnorm(length(extra))) +

    declare_inquiry(ATE = mean(extra1_Z_1) - mean(extra1_Z_0)) +
    declare_inquiry(ATT = mean(extra1_Z_1) - mean(extra1_Z_0), subset = group == 2) +

    declare_assignment(Z = complete_ra(N, prob = 0.5)) +
    declare_measurement(extra1 = reveal_outcomes(extra1 ~ Z)) +

    declare_estimator(extra1 ~ Z, .method = difference_in_means, inquiry = c("ATE", "ATT"), label = "DIM") +
    declare_estimator(extra1 ~ Z + group, .method = lm_robust, clusters = ID, inquiry = c("ATE", "ATT"), label = "OLS + control")

  diag <- diagnose_design(des, sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diag$diagnosands), 4)
})

