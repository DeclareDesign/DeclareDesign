test_that("want 1: the defaults are what you get for saying nothing", {
  d <- diagnose_design(simple_design(N = 30), sims = 5, bootstrap_sims = 0)
  expect_equal(d$diagnosand_names, names(default_diagnosands()))
  expect_equal(names(default_diagnosands()),
               c("mean_estimand", "mean_estimate", "bias", "sd_estimate",
                 "rmse", "power", "coverage"))
})

test_that("want 1: default_diagnosands takes alpha", {
  sims <- simulate_design(simple_design(N = 30, ate = 0), sims = 30)
  loose <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = default_diagnosands(alpha = 1))
  strict <- diagnose_simulations(sims, bootstrap_sims = 0,
                                 diagnosands = default_diagnosands(alpha = 0))
  expect_equal(get_diagnosands(loose)$power, 1)
  expect_equal(get_diagnosands(strict)$power, 0)
})

test_that("want 2: + adds a diagnosand to the defaults", {
  combined <- default_diagnosands() +
    declare_diagnosands(mae = mean(abs(estimate - estimand)))
  expect_s3_class(combined, "diagnosands")
  expect_equal(names(combined), c(names(default_diagnosands()), "mae"))

  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0,
                       diagnosands = combined)
  expect_true(all(c("bias", "mae") %in% names(get_diagnosands(d))))
  expect_gt(get_diagnosands(d)$mae, 0)
})

test_that("want 2: a repeated name takes its later definition, in place", {
  strict <- default_diagnosands() +
    declare_diagnosands(power = mean(p.value <= 0))
  # same names in the same order: overriding must not move the column
  expect_equal(names(strict), names(default_diagnosands()))

  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0,
                       diagnosands = strict)
  expect_equal(get_diagnosands(d)$power, 0)
})

test_that("want 2: + chains, and either side may be a custom set", {
  combined <- declare_diagnosands(a = mean(estimate)) +
    declare_diagnosands(b = stats::sd(estimate)) +
    declare_diagnosands(a = min(estimate))
  expect_equal(names(combined), c("a", "b"))
  expect_equal(rlang::as_label(rlang::quo_get_expr(combined$a)),
               "min(estimate)")
})

test_that("want 2: adding NULL leaves a diagnosands object alone", {
  expect_equal(names(default_diagnosands() + NULL),
               names(default_diagnosands()))
  expect_equal(names(NULL + default_diagnosands()),
               names(default_diagnosands()))
})

test_that("want 3: diagnosands takes a character vector of stock names", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0,
                       diagnosands = c("sd_estimate", "mean_se"))
  expect_equal(d$diagnosand_names, c("sd_estimate", "mean_se"))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("want 3: a single name works, and an unknown one is named", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0,
                       diagnosands = "power")
  expect_equal(d$diagnosand_names, "power")
  expect_error(diagnose_design(simple_design(N = 30), sims = 5,
                               diagnosands = c("bias", "bais")),
               "Unknown diagnosand: bais")
})

test_that("want 3: the same name means the same thing however it is asked for", {
  sims <- simulate_design(simple_design(N = 30), sims = 30)
  sims$estimate[1] <- NA_real_
  by_name <- diagnose_simulations(sims, bootstrap_sims = 0,
                                  diagnosands = "bias")
  by_object <- diagnose_simulations(sims, bootstrap_sims = 0,
                                    diagnosands = default_diagnosands())
  expect_equal(get_diagnosands(by_name)$bias, get_diagnosands(by_object)$bias)
  expect_false(is.na(get_diagnosands(by_name)$bias))
})

test_that("diagnosands are refused as a step of a design", {
  design <- simple_design(N = 30)
  expect_error(design + declare_diagnosands(cost = mean(estimate)),
               "not part of a design")
  expect_error(declare_diagnosands(cost = mean(estimate)) + design,
               "not part of a design")
  expect_error(declare_diagnosands(cost = mean(estimate)) +
                 declare_model(N = 5, Y = rnorm(N)),
               "not part of a design")
})

test_that("a bad diagnosands argument says what it will take", {
  expect_error(diagnose_design(simple_design(N = 30), sims = 5,
                               diagnosands = 42),
               "must be a diagnosands object or a character vector")
})

test_that("declare_diagnosands still honours subset and alpha", {
  sims <- simulate_design(simple_design(N = 30), sims = 30)
  counted <- diagnose_simulations(sims, bootstrap_sims = 0,
                                  diagnosands = declare_diagnosands(
                                    n = dplyr::n(), subset = p.value <= 0.05))
  expect_lt(get_diagnosands(counted)$n, 30L)

  tuned <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = declare_diagnosands(
                                  power = mean(p.value <= alpha), alpha = 1))
  expect_equal(get_diagnosands(tuned)$power, 1)
})

test_that("a diagnosands object prints its definitions", {
  expect_output(print(default_diagnosands()), "7 diagnosands")
  expect_output(print(declare_diagnosands(bias = mean(estimate - estimand))),
                "bias = mean\\(estimate - estimand\\)")
  expect_output(print(declare_diagnosands(n = dplyr::n(), subset = p.value < 1)),
                "on simulations where")
})

test_that("every diagnosand must be named", {
  expect_error(declare_diagnosands(mean(estimate)), "must be named")
})

test_that("stock_diagnosand_names lists what a character vector may hold", {
  expect_true(all(names(default_diagnosands()) %in% stock_diagnosand_names()))
  expect_length(stock_diagnosand_names(), 14L)
})

test_that("compare_diagnoses takes the same diagnosands spellings", {
  design <- simple_design(N = 40)
  cmp <- compare_diagnoses(design, redesign(design, N = 80),
                           sims = 20, bootstrap_sims = 20,
                           diagnosands = c("bias", "rmse"))
  expect_setequal(cmp$compared_diagnoses_df$diagnosand, c("bias", "rmse"))
})
