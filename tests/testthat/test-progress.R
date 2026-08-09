# Progress is signalled, never rendered by this package.
#
# progressr emits conditions and leaves the display to a handler the user
# installs. Two separate switches keep it quiet: with no handler nothing
# renders, and progressr itself defaults to `enable = interactive()`, so a
# knitted document, an Rscript and a CI job stay silent even if a handler is
# configured. The tests below pin both, and pin that the machinery does work
# once someone asks for it.

bar_frames <- function(expr) {
  out <- character(0)
  withr::with_options(list(progressr.enable = TRUE), {
    progressr::handlers(progressr::handler_txtprogressbar())
    # Assign the value: capture.output(type = "message") leaves the return
    # value to auto-print on stdout otherwise.
    out <- capture.output(res <- progressr::with_progress(expr), type = "message")
  })
  sum(grepl("[=|]|%", out))
}

simple <- function() {
  declare_model(N = 20, Y = rnorm(N)) + declare_inquiry(ATE = mean(Y))
}

test_that("nothing is emitted when no handler is installed", {
  skip_if_not_installed("progressr")
  out <- capture.output(
    res <- simulate_design(simple(), sims = 3),
    type = "message"
  )
  expect_equal(sum(nchar(out)), 0)
  expect_s3_class(res, "data.frame")
})

test_that("progress = TRUE stays silent in a non-interactive session", {
  # The property that makes this safe to leave on in a qmd or on CI.
  skip_if_not_installed("progressr")
  withr::with_options(list(progressr.enable = NULL), {
    out <- capture.output(
      res <- simulate_design(simple(), sims = 3, progress = TRUE),
      type = "message"
    )
    expect_equal(sum(nchar(out)), 0)
    expect_s3_class(res, "data.frame")
  })
})

test_that("progress renders once a handler and progressr are both enabled", {
  skip_if_not_installed("progressr")
  skip_if_not_installed("withr")
  expect_gt(bar_frames(simulate_design(simple(), sims = 5)), 0)
})

test_that("the option opts out even with a handler installed", {
  skip_if_not_installed("progressr")
  skip_if_not_installed("withr")
  withr::with_options(list(DeclareDesign.progress = FALSE), {
    expect_equal(bar_frames(simulate_design(simple(), sims = 5)), 0)
  })
})

test_that("diagnose_design carries the same argument", {
  skip_if_not_installed("progressr")
  expect_true("progress" %in% names(formals(diagnose_design)))
  out <- capture.output(
    res <- diagnose_design(simple(), sims = 3, bootstrap_sims = 0,
                           progress = TRUE),
    type = "message"
  )
  expect_equal(sum(nchar(out)), 0)
  expect_s3_class(get_diagnosands(res), "data.frame")
})

test_that("simulation still works when progressr is not installed", {
  # progressr is a Suggests. dd_progressor() has to degrade to a no-op rather
  # than error when it is absent.
  skip_if_not_installed("withr")
  tick <- withr::with_options(
    list(DeclareDesign.progress = FALSE),
    DeclareDesignZero:::dd_progressor(10, "x")
  )
  expect_true(is.function(tick))
  expect_null(tick())
})

test_that("progress does not change the result", {
  skip_if_not_installed("progressr")
  set.seed(1)
  a <- simulate_design(simple(), sims = 4)
  set.seed(1)
  b <- simulate_design(simple(), sims = 4, progress = TRUE)
  expect_equal(a, b)
})
