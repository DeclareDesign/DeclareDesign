# R/helpers.R: the internal helpers no verb's own file reaches directly.

test_that("pop.var is the mean squared deviation, not the corrected variance", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(pop.var(x), 2)
  expect_equal(pop.var(x), var(x) * (length(x) - 1) / length(x))
  # Both the mean and the deviations drop NA, so the divisor is the number of
  # values actually used.
  expect_equal(pop.var(c(1, 2, NA)), 0.25)
})

test_that("a simulation leaves a workspace that had no `.Random.seed` without one", {
  # Every draw runs on its own stream and the caller's generator is put back
  # as it was found. Having been found with no generator at all is a state the
  # restore has to reproduce: assigning a seed back would leave a workspace
  # changed by a function that promises not to change it.
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  if (had) rm(".Random.seed", envir = globalenv())
  saved <- save_rng_state()
  expect_null(saved$seed)
  simulate_design(simple_design(), sims = 2)
  restore_rng_state(saved)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("a loop with nothing to count gets no progressor", {
  # Asking progressr for a bar of zero steps is a question with no answer, so
  # the do-nothing closure is returned before progressr is reached.
  expect_null(dd_progressor(0)())
  expect_null(dd_progressor(NA_real_)())
})
