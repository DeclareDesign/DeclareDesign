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

test_that("run_design returns one data frame, not a list of three", {
  design <- simple_design(N = 40)
  one_run <- run_design(design)
  expect_s3_class(one_run, "data.frame")
  expect_equal(nrow(one_run), 1L)
  expect_true(all(c("inquiry", "estimand", "estimate") %in% names(one_run)))
  expect_false("sim_ID" %in% names(one_run))
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
  expect_error(run_design(6), "must be a `design` or `design_step`")
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

test_that("a failing inquiry is named the same way", {
  design <- declare_model(N = 20, U = rnorm(N)) +
    declare_inquiry(m = mean(W))
  err <- tryCatch(draw_estimands(design), error = function(e) e)
  expect_match(conditionMessage(err), "In step `m` \\(declare_inquiry\\(\\)\\)")
  expect_match(conditionMessage(err$parent), "object 'W' not found")
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

test_that("a parameter supplied at draw time reaches the design", {
  # Issue #497. The reporter's declaration leaves `theta` free deliberately,
  # because naming it above the design is what he is trying to avoid.
  model <- declare_model(N = 4000, e = rnorm(N), D = rbinom(N, 1, 0.5),
                         Y = theta * D + e)
  low <- draw_data(model, theta = 0.1)
  high <- draw_data(model, theta = 0.9)
  expect_equal(nrow(low), 4000L)
  gap <- function(df) mean(df$Y[df$D == 1]) - mean(df$Y[df$D == 0])
  expect_equal(gap(low), 0.1, tolerance = 0.1)
  expect_equal(gap(high), 0.9, tolerance = 0.1)
})

test_that("draw-time parameters work on every verb in the family", {
  design <- declare_model(N = 200, e = rnorm(N), D = rbinom(N, 1, 0.5),
                          Y = theta * D + e) +
    declare_inquiry(ATE = theta) +
    declare_estimator(Y ~ D, .method = lm, term = "D", inquiry = "ATE",
                      label = "ols")
  expect_equal(nrow(draw_data(design, theta = 0.5)), 200L)
  expect_equal(draw_estimands(design, theta = 0.5)$estimand, 0.5)
  expect_equal(draw_estimates(design, theta = 0.5)$estimand, 0.5)
  expect_equal(run_design(design, theta = 0.5)$estimand, 0.5)
})

test_that("a sweep supplied at draw time is refused, not silently narrowed", {
  model <- declare_model(N = 20, D = rbinom(N, 1, 0.5), Y = theta * D + rnorm(N))
  expect_error(draw_data(model, theta = c(0.1, 0.5)),
               "gives 2 designs, and this verb draws from one")
})

test_that("drawing without parameters is unchanged", {
  design <- declare_model(N = 25, X = rnorm(N)) + declare_inquiry(m = mean(X))
  expect_equal(nrow(draw_data(design)), 25L)
  expect_equal(nrow(draw_estimands(design)), 1L)
})
