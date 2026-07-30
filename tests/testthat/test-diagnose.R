test_that("diagnose_design accepts custom diagnosands via set_diagnosands", {
  design <- simple_design(N = 30) |>
    set_diagnosands(declare_diagnosands(
      mean_estimate = mean(estimate, na.rm = TRUE)
    ))
  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_true("mean_estimate" %in% names(get_diagnosands(d)))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("select_diagnosands subsets diagnosands", {
  diags <- default_diagnosands()
  trimmed <- select_diagnosands(diags, "bias", "rmse")
  expect_equal(names(attr(trimmed, "dots")), c("bias", "rmse"))
})

test_that("select_diagnosands builds a set from the library, as in DeclareDesign", {
  diags <- select_diagnosands("sd_estimate", "mean_se")
  expect_equal(names(attr(diags, "dots")), c("sd_estimate", "mean_se"))
  d <- diagnose_design(simple_design(N = 30), diagnosands = diags, sims = 10,
                       bootstrap_sims = 0)
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("inquiry", "estimator", "outcome", "term", "n_sims")),
               c("sd_estimate", "mean_se"))
})

test_that("select_diagnosands reaches diagnosands outside the default set", {
  extra <- c("type_s_rate", "exaggeration_ratio", "var_estimate",
             "mean_var_hat", "prop_pos_sig", "mean_ci_length", "mean_estimand")
  diags <- rlang::inject(select_diagnosands(!!!extra))
  expect_equal(names(attr(diags, "dots")), extra)
})

test_that("select_diagnosands names an unknown diagnosand rather than ignoring it", {
  expect_error(select_diagnosands("bais"), "Unknown diagnosand")
  expect_error(select_diagnosands(default_diagnosands(), "bais"),
               "not in this set")
})

test_that("select_diagnosands passes alpha through to power", {
  sims <- simulate_design(simple_design(N = 30, ate = 0), sims = 30)
  loose <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = select_diagnosands("power", alpha = 1))
  strict <- diagnose_simulations(sims, bootstrap_sims = 0,
                                 diagnosands = select_diagnosands("power", alpha = 0))
  expect_equal(get_diagnosands(loose)$power, 1)
  expect_equal(get_diagnosands(strict)$power, 0)
})

test_that("a diagnosands subset restricts which simulations count", {
  sims <- simulate_design(simple_design(N = 30), sims = 30)
  all_sims <- diagnose_simulations(sims, bootstrap_sims = 0,
                                   diagnosands = declare_diagnosands(
                                     n = dplyr::n()))
  significant <- diagnose_simulations(sims, bootstrap_sims = 0,
                                      diagnosands = declare_diagnosands(
                                        n = dplyr::n(), subset = p.value <= 0.05))
  expect_equal(get_diagnosands(all_sims)$n, 30L)
  expect_lt(get_diagnosands(significant)$n, 30L)
})

test_that("declare_diagnosands binds alpha inside diagnosand expressions", {
  design <- simple_design(N = 30, ate = 0)
  sims <- simulate_design(design, sims = 30)
  d <- diagnose_simulations(sims, bootstrap_sims = 0,
                            diagnosands = declare_diagnosands(
                              power = mean(p.value <= alpha), alpha = 1))
  expect_equal(get_diagnosands(d)$power, 1)
})

test_that("diagnose_design over multiple designs adds a design column", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  d <- diagnose_design(!!!fam, sims = 5, bootstrap_sims = 0)
  diag <- get_diagnosands(d)
  expect_true("design" %in% names(diag))
  expect_equal(nrow(diag), 2L)
})

test_that("modify_design (insert/delete/replace) still works after deprecation", {
  design <- declare_model(N = 30, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))

  inserted <- suppressWarnings(insert_step(design, declare_measurement(Y2 = Y * 2), after = "model"))
  expect_equal(length(inserted), 3L)

  deleted <- suppressWarnings(delete_step(design, "mu"))
  expect_equal(length(deleted), 1L)

  replaced <- suppressWarnings(replace_step(design, "mu", declare_inquiry(med = median(Y))))
  expect_equal(length(replaced), 2L)
})

test_that("bootstrap tolerates diagnosands that cannot be computed", {
  # Regression test. The point estimate wrapped each diagnosand in a tryCatch
  # and the bootstrap did not, so a design with no inquiry diagnosed at
  # bootstrap_sims = 0 and errored at the default of 100.
  design <- declare_model(N = 60, Y = rbinom(N, 1, 0.55)) +
    declare_test(Y ~ 1, .method = lm, term = "(Intercept)", label = "t")
  diag <- diagnose_design(design, sims = 10, bootstrap_sims = 10)
  diag_df <- get_diagnosands(diag)
  expect_true(all(is.na(diag_df$bias)))
  expect_true(is.finite(diag_df$mean_estimate))
  expect_true("se(mean_estimate)" %in% names(diag_df))
})

test_that("legacy `model =` is read as `.method` with a deprecation warning", {
  skip_if_not_installed("estimatr")
  expect_warning(
    step <- declare_estimator(Y ~ Z, model = estimatr::lm_robust, term = "Z",
                              label = "ols"),
    "deprecated"
  )
  expect_identical(attr(step, "method_name"), "lm_robust")
  design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20),
                          Y = U + 0.5 * Z) + step
  est <- draw_estimates(design)
  expect_equal(nrow(est), 1L)
  expect_false("model" %in% names(est))
})

test_that("select_diagnosands refuses library arguments when subsetting a set", {
  diags <- declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
  expect_error(select_diagnosands(diags, "power", alpha = 0.5),
               "cannot be applied to a diagnosands set that already exists")
  expect_error(select_diagnosands(diags, "power", subset = p.value < 1),
               "cannot be applied")
  expect_no_error(select_diagnosands(diags, "power"))
})

test_that("select_diagnosands says so when the step is not a diagnosands set", {
  expect_error(select_diagnosands(declare_model(N = 5, Y = rnorm(N)), "bias"),
               "this is a model step")
})

test_that("reshape_diagnosis gives columns their display names", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  out <- reshape_diagnosis(d)
  expect_true(all(c("Inquiry", "Estimator", "Term", "N Sims",
                    "Mean Estimand", "SD Estimate", "RMSE", "Power") %in%
                    names(out)))
  expect_equal(nrow(out), 1L)
  expect_type(out[["Bias"]], "character")
})

test_that("reshape_diagnosis puts bootstrap SEs in parentheses below", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 10)
  out <- reshape_diagnosis(d)
  expect_equal(nrow(out), 2L)
  expect_match(out[["Bias"]][2], "^\\(.*\\)$")
  expect_equal(out[["Inquiry"]][2], "")
})

test_that("reshape_diagnosis selects and excludes by display name", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  expect_equal(names(reshape_diagnosis(d, select = c("Bias", "Power"))),
               c("Inquiry", "Estimator", "Term", "N Sims", "Bias", "Power"))
  expect_false("Coverage" %in% names(reshape_diagnosis(d, exclude = "Coverage")))
  expect_error(reshape_diagnosis(d, select = "bias"), "must name diagnosands")
})

test_that("reshape_diagnosis leaves redesign parameter names alone", {
  designs <- redesign(simple_design(N = 30), ate = c(0.1, 0.5))
  d <- diagnose_design(designs, sims = 5, bootstrap_sims = 0)
  out <- reshape_diagnosis(d)
  expect_true("ate" %in% names(out))
})

test_that("tidy(diagnosis) carries the bootstrap interval", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 20)
  td <- tidy(d)
  expect_true(all(c("diagnosand", "estimate", "std.error", "conf.low",
                    "conf.high") %in% names(td)))
  expect_false(any(grepl("^se\\(", names(td))))
  bias <- td[td$diagnosand == "bias", ]
  expect_true(bias$conf.low <= bias$estimate)
  expect_true(bias$estimate <= bias$conf.high)
  expect_false("conf.low" %in% names(tidy(d, conf.int = FALSE)))
})

test_that("tidy(diagnosis) works without a bootstrap", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  td <- tidy(d)
  expect_true(all(c("diagnosand", "estimate") %in% names(td)))
  expect_false("std.error" %in% names(td))
})

test_that("designs supplied in a list keep the list's own names", {
  designs <- list(dum = simple_design(N = 30), dee = simple_design(N = 30))
  d <- diagnose_design(designs, sims = 5, bootstrap_sims = 0)
  expect_setequal(get_diagnosands(d)$design, c("dum", "dee"))
  expect_setequal(reshape_diagnosis(d)[["Design"]], c("dum", "dee"))
  sims <- simulate_design(designs, sims = 3)
  expect_setequal(sims$design, c("dum", "dee"))
})

test_that("designs supplied as bare symbols are named for the symbol", {
  dum <- simple_design(N = 30)
  dee <- simple_design(N = 30)
  sims <- simulate_design(dum, dee, sims = 3)
  expect_setequal(sims$design, c("dum", "dee"))
})
