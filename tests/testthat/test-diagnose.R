test_that("diagnose_design accepts a custom diagnosands object", {
  d <- diagnose_design(simple_design(N = 30), sims = 5, bootstrap_sims = 0,
                       diagnosands = declare_diagnosands(
                         mean_estimate = mean(estimate, na.rm = TRUE)
                       ))
  expect_true("mean_estimate" %in% names(get_diagnosands(d)))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("set_diagnosands says diagnosands are not part of a design", {
  expect_error(set_diagnosands(simple_design(N = 30), default_diagnosands()),
               "not part of a design")
})

test_that("select_diagnosands still works, with a deprecation warning", {
  # the warning is once-per-session, so ask for it every time here
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(select_diagnosands("bias", "rmse"), "deprecated")
  trimmed <- suppressWarnings(select_diagnosands(default_diagnosands(),
                                                 "bias", "rmse"))
  expect_equal(names(trimmed), c("bias", "rmse"))
})

test_that("select_diagnosands builds a set from the library, as in DeclareDesign", {
  diags <- suppressWarnings(select_diagnosands("sd_estimate", "mean_se"))
  expect_equal(names(diags), c("sd_estimate", "mean_se"))
  d <- diagnose_design(simple_design(N = 30), diagnosands = diags, sims = 10,
                       bootstrap_sims = 0)
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("inquiry", "estimator", "outcome", "term")),
               c("sd_estimate", "mean_se"))
})

test_that("select_diagnosands reaches diagnosands outside the default set", {
  extra <- c("type_s_rate", "exaggeration_ratio", "var_estimate",
             "mean_var_hat", "prop_pos_sig", "mean_ci_length", "mean_estimand")
  diags <- suppressWarnings(rlang::inject(select_diagnosands(!!!extra)))
  expect_equal(names(diags), extra)
})

test_that("select_diagnosands names an unknown diagnosand rather than ignoring it", {
  expect_error(suppressWarnings(select_diagnosands("bais")), "Unknown diagnosand")
  expect_error(suppressWarnings(select_diagnosands(default_diagnosands(), "bais")),
               "not in this set")
})

test_that("select_diagnosands passes alpha through to power", {
  sims <- simulate_design(simple_design(N = 30, ate = 0), sims = 30)
  loose <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = default_diagnosands(alpha = 1))
  strict <- diagnose_simulations(sims, bootstrap_sims = 0,
                                 diagnosands = default_diagnosands(alpha = 0))
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
