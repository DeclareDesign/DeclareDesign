test_that("compare_diagnoses reports both designs and their difference", {
  design <- simple_design(N = 40)
  cmp <- compare_diagnoses(design, redesign(design, N = 400),
                           sims = 30, bootstrap_sims = 30)
  expect_s3_class(cmp, "compared_diagnoses")
  df <- cmp$compared_diagnoses_df
  expect_true(all(c("diagnosand", "mean_1", "mean_2", "mean_difference",
                    "se_1", "se_2", "se_difference", "conf.low", "conf.high")
                  %in% names(df)))
  expect_setequal(df$diagnosand, cmp$diagnosis1$diagnosand_names)
  expect_equal(df$mean_difference, df$mean_2 - df$mean_1)
  # The larger design estimates the ATE more precisely.
  expect_lt(df$mean_difference[df$diagnosand == "sd_estimate"], 0)
  expect_output(print(cmp), "design 2 minus design 1")
})

test_that("compare_diagnoses accepts diagnoses that have already been run", {
  design <- simple_design(N = 40)
  d1 <- diagnose_design(design, sims = 20, bootstrap_sims = 20)
  d2 <- diagnose_design(redesign(design, N = 80), sims = 20, bootstrap_sims = 20)
  cmp <- compare_diagnoses(d1, d2)
  expect_identical(cmp$diagnosis1$diagnosands_df, d1$diagnosands_df)
  expect_equal(nrow(cmp$compared_diagnoses_df), length(d1$diagnosand_names))
})

test_that("compare_diagnoses without a bootstrap reports differences and no intervals", {
  design <- simple_design(N = 40)
  cmp <- compare_diagnoses(design, redesign(design, N = 80),
                           sims = 20, bootstrap_sims = 0)
  df <- cmp$compared_diagnoses_df
  expect_false(any(is.na(df$mean_difference)))
  expect_true(all(is.na(df$se_difference)))
})

test_that("merge_by_estimator = FALSE crosses the estimators of the two designs", {
  design <- simple_design(N = 40)
  two <- design + declare_estimator(Y ~ Z, .method = lm, term = "Z",
                                    inquiry = "ATE", label = "ols2")
  matched <- compare_diagnoses(design, two, sims = 20, bootstrap_sims = 20)
  crossed <- compare_diagnoses(design, two, sims = 20, bootstrap_sims = 20,
                               merge_by_estimator = FALSE)
  expect_true("estimator" %in% names(matched$compared_diagnoses_df))
  expect_true(all(c("estimator_1", "estimator_2") %in%
                    names(crossed$compared_diagnoses_df)))
  expect_equal(nrow(crossed$compared_diagnoses_df),
               2L * nrow(matched$compared_diagnoses_df))
})

test_that("compare_diagnoses refuses designs with nothing to match on", {
  design <- simple_design(N = 40)
  no_labels <- declare_model(N = 40, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_error(compare_diagnoses(design, no_labels, sims = 10,
                                 bootstrap_sims = 0),
               "no labels in common")
  expect_error(compare_diagnoses(design, "not a design"),
               "must be a `design` or a `diagnosis`")
})

test_that("compare_diagnoses applies one set of diagnosands to both designs", {
  design <- simple_design(N = 40)
  cmp <- compare_diagnoses(design, redesign(design, N = 80),
                           sims = 20, bootstrap_sims = 20,
                           diagnosands = declare_diagnosands(
                             bias = mean(estimate - estimand),
                             spread = sd(estimate)))
  expect_setequal(cmp$compared_diagnoses_df$diagnosand, c("bias", "spread"))
  expect_equal(cmp$diagnosis1$diagnosand_names, cmp$diagnosis2$diagnosand_names)
})
