# Regression tests for patterns drawn directly from book.declaredesign.org.
# Each test exercises a code idiom from the book chapters; if any of these
# break, the package has lost drop-in compatibility with DeclareDesign for
# the canonical examples readers encounter first.

skip_if_no_estimatr <- function() {
  testthat::skip_if_not_installed("estimatr")
  testthat::skip_if_not_installed("randomizr")
  testthat::skip_if_not_installed("fabricatr")
}

test_that("formula passed via `Y ~ Z` reaches lm_robust as a real formula", {
  skip_if_no_estimatr()
  design <- declare_model(N = 60, U = rnorm(N),
                          fabricatr::potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = randomizr::complete_ra(N, prob = 0.5)) +
    declare_measurement(Y = fabricatr::reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  ests <- draw_estimates(design)
  expect_true("estimate" %in% names(ests))
  expect_equal(unique(ests$inquiry), "ATE")
  # Default term filter should drop the (Intercept) row.
  expect_false(any(ests$term == "(Intercept)"))
})

test_that("declare_estimands and declare_estimand are aliases for declare_inquiry", {
  d1 <- declare_inquiry(mu = mean(Y))
  d2 <- declare_estimand(mu = mean(Y))
  d3 <- declare_estimands(mu = mean(Y))
  expect_identical(attr(d1, "step_type"), "inquiry")
  expect_identical(attr(d2, "step_type"), "inquiry")
  expect_identical(attr(d3, "step_type"), "inquiry")
})

test_that("multiple inquiries in one declare_inquiry produce one row per name", {
  design <- declare_model(N = 50, Y_Z_1 = 1, Y_Z_0 = 0) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0),
                    ATT = mean(Y_Z_1) - mean(Y_Z_0))
  inq <- draw_estimands(design)
  expect_equal(nrow(inq), 2L)
  expect_setequal(inq$inquiry, c("ATE", "ATT"))
})

test_that("inquiry subset filters before estimand evaluation", {
  design <- declare_model(N = 100, X = rep(0:1, each = 50), Y = X * 1.0) +
    declare_inquiry(MeanY = mean(Y), subset = X == 1)
  inq <- draw_estimands(design)
  expect_equal(inq$estimand, 1)
})

test_that("redesign substitutes parameters captured as free symbols", {
  skip_if_no_estimatr()
  declaration <-
    declare_model(N = N, U = rnorm(N),
                  fabricatr::potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = randomizr::complete_ra(N = N, prob = prob)) +
    declare_measurement(Y = fabricatr::reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  designs <- redesign(declaration, N = c(50, 100), prob = c(0.3, 0.5))
  expect_length(designs, 4L)
  # Each design must produce a non-zero data frame
  for (d in designs) {
    df <- draw_data(d)
    expect_gt(nrow(df), 0)
  }
})

test_that("redesign parameters surface as columns in simulations and diagnosands", {
  skip_if_no_estimatr()
  d <- declare_model(N = N, U = rnorm(N),
                     fabricatr::potential_outcomes(Y ~ 0.2 * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = randomizr::complete_ra(N = N, prob = 0.5)) +
    declare_measurement(Y = fabricatr::reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  designs <- redesign(d, N = c(50, 100))
  sims <- simulate_design(designs, sims = 3)
  expect_true("N" %in% names(sims))
  diag <- diagnose_design(
    designs,
    diagnosands = declare_diagnosands(power = mean(p.value <= 0.05),
                                       cost  = unique(N * 2)),
    sims = 3, bootstrap_sims = 0
  )
  diag_df <- get_diagnosands(diag)
  expect_true("cost" %in% names(diag_df))
  expect_true("N" %in% names(diag_df))
})

test_that("default diagnosands tolerate designs with no inquiry", {
  skip_if_no_estimatr()
  design <- declare_model(N = 80) +
    declare_measurement(Y = rbinom(n = N, size = 1, prob = 0.55)) +
    declare_test(handler = label_estimator(function(data) {
      test <- prop.test(x = table(data$Y), p = 0.5)
      broom::tidy(test)
    }))
  diag <- diagnose_design(design, sims = 3, bootstrap_sims = 0)
  diag_df <- get_diagnosands(diag)
  # Bias / coverage have nothing to compute here, but the call must not error.
  expect_true("power" %in% names(diag_df))
  expect_true(all(is.na(diag_df$bias)))
})

test_that("declare_inquiry handler form (e.g. tibble) supports forward-referencing args", {
  skip_if_no_estimatr()
  x_range <- 1:5
  design <- declare_model(N = 30, X = runif(N, 0, 1)) +
    declare_inquiry(
      X        = x_range,
      inquiry  = paste0("X_", X),
      estimand = X * 2,
      data     = NULL,
      handler  = tibble::tibble
    )
  inq <- draw_estimands(design)
  expect_equal(nrow(inq), length(x_range))
  expect_equal(inq$inquiry, paste0("X_", x_range))
  expect_equal(inq$estimand, x_range * 2)
})

test_that("diagnose_design accepts a list of designs and adds a `design` column", {
  skip_if_no_estimatr()
  d1 <- simple_design(N = 30, ate = 0.0)
  d2 <- simple_design(N = 30, ate = 0.5)
  diag <- diagnose_design(list(d1, d2), sims = 3, bootstrap_sims = 0)
  diag_df <- get_diagnosands(diag)
  expect_true("design" %in% names(diag_df))
  expect_equal(length(unique(diag_df$design)), 2L)
})

test_that("simulate_design accepts a list of redesigned designs", {
  skip_if_no_estimatr()
  d <- simple_design(N = 30)
  designs <- redesign(d, ate = c(0, 0.3))
  sims <- simulate_design(designs, sims = 2)
  expect_true("design" %in% names(sims))
  expect_true("ate" %in% names(sims))
})

test_that("expand_design varies parameters via a designer function", {
  designer <- function(N) {
    declare_model(N = N, Y = rnorm(N)) +
      declare_inquiry(mu = mean(Y))
  }
  designs <- suppressWarnings(expand_design(designer, N = c(10, 20, 30)))
  expect_length(designs, 3L)
  expect_equal(nrow(draw_data(designs[[1]])), 10L)
  expect_equal(nrow(draw_data(designs[[3]])), 30L)
})

test_that("insert_step / replace_step / delete_step still work after deprecation", {
  d <- declare_model(N = 20, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))
  d_added <- suppressWarnings(insert_step(d, declare_measurement(Y2 = Y * 2), after = "model"))
  expect_length(d_added, 3L)
  expect_true("measurement" %in% names(d_added))
  d_replaced <- suppressWarnings(replace_step(d, "mu", declare_inquiry(med = stats::median(Y))))
  expect_equal(draw_estimands(d_replaced)$inquiry, "med")
  d_deleted <- suppressWarnings(delete_step(d, "mu"))
  expect_length(d_deleted, 1L)
})

test_that("set_diagnosands stores diagnosands used by diagnose_design", {
  skip_if_no_estimatr()
  d <- simple_design(N = 30)
  d <- set_diagnosands(d, declare_diagnosands(power = mean(p.value <= 0.05)))
  diag <- diagnose_design(d, sims = 3, bootstrap_sims = 0)
  diag_df <- get_diagnosands(diag)
  expect_true("power" %in% names(diag_df))
  expect_false("bias" %in% names(diag_df))
})

test_that("select_diagnosands subsets the diagnosand set", {
  diags <- default_diagnosands()
  kept <- select_diagnosands(diags, "bias", "rmse")
  expect_setequal(names(attr(kept, "dots")), c("bias", "rmse"))
})

test_that("tidy(diagnosis) reshapes diagnosands long", {
  skip_if_no_estimatr()
  d <- simple_design(N = 30)
  diag <- diagnose_design(d, sims = 3, bootstrap_sims = 0)
  td <- generics::tidy(diag)
  expect_true("diagnosand" %in% names(td))
  expect_true("estimate" %in% names(td))
})

test_that("get_simulations and get_diagnosands return tibbles", {
  skip_if_no_estimatr()
  d <- simple_design(N = 30)
  diag <- diagnose_design(d, sims = 3, bootstrap_sims = 0)
  expect_s3_class(get_simulations(diag), "tbl_df")
  expect_s3_class(get_diagnosands(diag), "tbl_df")
})

test_that("label_estimator wraps a custom function for declare_estimator", {
  skip_if_no_estimatr()
  my_est <- label_estimator(
    function(data, ...) lm(Y ~ Z, data = data),
    label = "ols", inquiry = "ATE", term = "Z"
  )
  design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15),
                          Y = U + 0.5 * Z) +
    declare_inquiry(ATE = 0.5) +
    declare_estimator(handler = my_est)
  ests <- draw_estimates(design)
  expect_equal(unique(ests$estimator), "ols")
  expect_equal(unique(ests$inquiry), "ATE")
})

test_that("declare_step composes a custom data-mutating step", {
  step <- declare_step(handler = function(data, k) {
    data$Y2 <- data$Y * k
    data
  }, k = 3)
  design <- declare_model(N = 5, Y = 1:5) + step
  df <- draw_data(design)
  expect_equal(df$Y2, 1:5 * 3)
})
