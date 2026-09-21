# Running a design once: `run_design()` and the `draw_*()` family, plus
# `get_estimates()`, which runs the answer strategy against data it is handed
# rather than data it draws.
#
# Failed estimators are test-estimator-failures.R. What `simulate_design()`
# does with many runs is test-simulate_design.R.

two_arm <- function(N = 40) {
  declare_model(N = N, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
}

test_that("run_design returns one row per estimate with the estimand joined", {
  out <- run_design(two_arm())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_true(all(c("estimator", "term", "estimate", "inquiry", "estimand")
                  %in% names(out)))
  expect_equal(out$inquiry, "ATE")
  expect_equal(out$estimand, 0.5)
})

test_that("run_design accepts a bare step, not only a design", {
  # construct_design(wrap_step(.)) is the path a one-step design takes, and
  # `declare_model(N = 10) + NULL` is not how anyone writes it by hand.
  step <- declare_model(N = 10, U = rnorm(N))
  expect_equal(nrow(draw_data(step)), 10L)
  expect_equal(nrow(run_design(step)), 0L)
})

test_that("run_design refuses something that is not a design", {
  expect_error(run_design(tibble::tibble(x = 1)),
               "must be a `design` or `design_step`")
  expect_error(run_design("Y ~ Z"), "must be a `design` or `design_step`")
})

test_that("a failing step is named before its own error is re-raised", {
  design <- declare_model(N = 10, U = rnorm(N)) +
    declare_measurement(Y = W + 1, label = "reveal")
  err <- expect_error(draw_data(design))
  expect_match(conditionMessage(err), "reveal")
  expect_match(conditionMessage(err), "measurement")
  # The original condition survives as the parent, so a caller catching by
  # class still can.
  expect_s3_class(err$parent, "condition")
})

test_that("draw_data runs the data steps and nothing else", {
  design <- declare_model(N = 20, U = rnorm(N), Y = U) +
    declare_inquiry(mu = stop("the inquiry ran")) +
    declare_estimator(Y ~ 1, .method = lm, label = "never")
  df <- draw_data(design)
  expect_equal(nrow(df), 20L)
  expect_true(all(c("ID", "U", "Y") %in% names(df)))
})

test_that("draw_estimands runs the inquiries and not the estimators", {
  design <- declare_model(N = 20, U = rnorm(N), Y = U) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ 1, .method = function(...) stop("the estimator ran"),
                      label = "never")
  inq <- draw_estimands(design)
  expect_s3_class(inq, "tbl_df")
  expect_equal(inq$inquiry, "mu")
  expect_equal(nrow(inq), 1L)
})

test_that("draw_estimand is draw_estimands under its 1.x name", {
  expect_identical(draw_estimand, draw_estimands)
})

test_that("draw_estimates joins the estimand and drops the data", {
  out <- draw_estimates(two_arm())
  expect_equal(nrow(out), 1L)
  expect_equal(out$estimand, 0.5)
  expect_false("U" %in% names(out))
})

test_that("get_estimates estimates on the data it is handed", {
  # The whole point: the answer comes from this data frame, not from a fresh
  # draw. A design that re-drew would give a different number every call.
  design <- two_arm()
  df <- draw_data(design)
  out <- get_estimates(design, df)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_equal(out$estimate, unname(coef(lm(Y ~ Z, data = df))[["Z"]]))
  expect_identical(get_estimates(design, df)$estimate, out$estimate)
})

test_that("get_estimates runs no data step, so the data is never rebuilt", {
  design <- declare_model(N = 5, U = rnorm(N), Y = U) +
    declare_model(Y2 = stop("the second model step ran"), label = "second") +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", label = "ols")
  df <- tibble::tibble(Y = c(1, 2, 3, 4))
  out <- get_estimates(design, df)
  expect_equal(out$estimate, 2.5)
})

test_that("get_estimates draws its own data when none is supplied", {
  out <- get_estimates(two_arm())
  expect_equal(nrow(out), 1L)
  expect_true(is.finite(out$estimate))
  # No inquiry is joined: get_estimates runs the estimator steps alone.
  expect_false("estimand" %in% names(out))
})

test_that("get_estimates windows the design with start and end", {
  design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15),
                          Y = U + Z) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "first") +
    declare_estimator(Y ~ Z, .method = lm, term = "(Intercept)",
                      label = "second")
  df <- draw_data(design)
  expect_setequal(get_estimates(design, df)$estimator, c("first", "second"))
  expect_equal(get_estimates(design, df, end = 2L)$estimator, "first")
  expect_equal(get_estimates(design, df, start = 3L)$estimator, "second")
})

test_that("get_estimates reads a note against the data it was given", {
  # Documented behaviour rather than an accident: the note has no other data
  # to read, so a design whose note must be computed before sampling is
  # better run than re-estimated.
  design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
    declare_notes(n_seen = nrow(data)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
  df <- draw_data(design)[1:10, ]
  expect_equal(nrow(get_estimates(design, df)), 1L)
  expect_equal(design_notes(design)$name, "n_seen")
})

test_that("get_estimates accepts a bare step", {
  step <- declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                            label = "ols")
  out <- get_estimates(step, tibble::tibble(Y = c(2, 4, 6)))
  expect_equal(out$estimate, 4)
})

test_that("a design with no estimator returns zero estimate rows", {
  design <- declare_model(N = 10, U = rnorm(N)) + declare_inquiry(mu = mean(U))
  expect_equal(nrow(get_estimates(design, draw_data(design))), 0L)
  expect_equal(nrow(run_design(design)), 1L)
})
