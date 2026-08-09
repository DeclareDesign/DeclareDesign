# An estimator that fails on one draw must not take the run with it, and the
# failure must not be quiet. Draws that fail are not missing at random: a fit
# that will not converge is a fit on an awkward draw, so diagnosing on the
# survivors flatters the design. Everything here is about the accounting.

flaky <- function(fail_on) {
  i <- 0L
  function(data) {
    i <<- i + 1L
    if (i %in% fail_on) stop("model did not converge")
    estimatr::lm_robust(Y ~ Z, data = data)
  }
}

two_estimator_design <- function(handler) {
  declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.3) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(Y ~ Z, .method = estimatr::lm_robust, term = "Z",
                      inquiry = "ATE", label = "reliable") +
    declare_estimator(handler = label_estimator(handler, label = "flaky",
                                                inquiry = "ATE", term = "Z"),
                      label = "flaky")
}

test_that("one failing draw does not abort the run", {
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = 2L))
  sims <- suppressWarnings(simulate_design(design, sims = 4))
  expect_s3_class(sims, "data.frame")
  expect_equal(sum(sims$estimator == "reliable"), 4)
})

test_that("the failure is recorded, with its message", {
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = c(1L, 3L)))
  sims <- suppressWarnings(simulate_design(design, sims = 4))
  failed <- sims[!is.na(sims$error) & sims$error, ]
  expect_equal(nrow(failed), 2)
  expect_true(all(failed$estimator == "flaky"))
  expect_true(all(grepl("did not converge", failed$error_message)))
  expect_true(all(is.na(failed$estimate)))
})

test_that("one warning per run, naming the estimator and the count", {
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = c(1L, 2L, 3L)))
  ws <- character(0)
  withCallingHandlers(
    simulate_design(design, sims = 5),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(ws, 1)
  expect_match(ws, "3 estimator draws failed")
  expect_match(ws, "flaky")
  expect_match(ws, "did not converge")
})

test_that("a run with no failures warns not at all", {
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = integer(0)))
  ws <- character(0)
  withCallingHandlers(
    simulate_design(design, sims = 3),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(ws, 0)
})

test_that("n_sims counts the draws a diagnosand actually used", {
  # The point of the whole exercise. If the failed rows were summarised
  # instead of dropped, n_sims would read 6 for both estimators while every
  # default diagnosand, all of which carry na.rm = TRUE, computed on fewer.
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = c(1L, 2L)))
  d <- suppressWarnings(
    get_diagnosands(diagnose_design(design, sims = 6, bootstrap_sims = 0))
  )
  expect_equal(d$n_sims[d$estimator == "reliable"], 6)
  expect_equal(d$n_sims[d$estimator == "flaky"], 4)
  expect_false(any(is.na(d$bias)))
})

test_that("a design whose only estimator always fails still diagnoses", {
  skip_if_not_installed("estimatr")
  design <- declare_model(N = 30, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.3) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(handler = label_estimator(
      function(data) stop("always"), label = "doomed", inquiry = "ATE"),
      label = "doomed")
  sims <- suppressWarnings(simulate_design(design, sims = 3))
  expect_equal(sum(sims$error, na.rm = TRUE), 3)
})

test_that("a single run re-raises rather than returning an NA row", {
  # Tolerance is for the simulation loop. Somebody calling draw_estimates() is
  # debugging, and an error message buried in a column of an NA row is a worse
  # answer than the error. Two tests elsewhere in this suite depend on this
  # (test-basic_workflow.R and test-declare_estimator.R), and they are what
  # caught the first version of this feature swallowing errors everywhere.
  skip_if_not_installed("estimatr")
  # A fresh design per assertion: flaky() counts its own calls, so reusing one
  # would let the first assertion consume the failing draw.
  expect_error(draw_estimates(two_estimator_design(flaky(fail_on = 1L))),
               "did not converge")
  expect_error(run_design(two_estimator_design(flaky(fail_on = 1L))),
               "did not converge")
})

test_that("the same design tolerates the same failure under simulation", {
  skip_if_not_installed("estimatr")
  design <- two_estimator_design(flaky(fail_on = 1L))
  sims <- suppressWarnings(simulate_design(design, sims = 3))
  expect_equal(sum(sims$error, na.rm = TRUE), 1)
  expect_equal(sum(sims$estimator == "reliable"), 3)
})
