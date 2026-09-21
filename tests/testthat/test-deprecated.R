# R/deprecated.R: the 1.x spellings, and what each one does now.
#
# These tests lived in test-autolabel.R, which is a file about estimator
# labelling and has nothing to do with them.
#
# Three strata, and the third is the one that was missing. `declare_population()`,
# `model_handler()` and `tidy_estimator()` warn and forward. The compare_*
# family and `print_code()` are defunct and error. `declare_potential_outcomes()`
# and `declare_reveal()` were at 0% coverage: both exist only to error with a
# message naming the replacement, and nothing asserted that they do.
#
# An error whose whole purpose is to say what to write instead is only as good
# as the advice, so each replacement spelling is run here too. Nothing else
# checks it: the 2.0 vignette prints these messages, but it sets eval = FALSE
# for the whole document, so every output in it is hand-written.

# Deprecated: warn once, then forward ----

test_that("declare_population() warns and forwards to declare_model()", {
  expect_warning(m <- declare_population(N = 10, Y = rnorm(N)), "deprecated")
  expect_s3_class(m, "design_step")
  expect_equal(nrow(draw_data(suppressWarnings(declare_population(N = 10, Y = rnorm(N))))), 10L)
})

test_that("model_handler() and tidy_estimator() warn and forward to label_estimator()", {
  expect_warning(
    fn <- model_handler(function(data, ...) lm(Y ~ Z, data = data),
                        label = "lm", inquiry = "ATE", term = "Z"),
    "deprecated"
  )
  expect_true(is.function(fn))
  expect_warning(
    fn2 <- tidy_estimator(function(data, ...) lm(Y ~ Z, data = data),
                          label = "lm", inquiry = "ATE", term = "Z"),
    "deprecated"
  )
  expect_true(is.function(fn2))
})

# Defunct: error, naming what to use instead ----

test_that("the compare_* family and print_code() error informatively", {
  expect_error(compare_designs(), "not implemented")
  expect_error(compare_design_code(), "not implemented")
  expect_error(compare_design_data(), "not implemented")
  expect_error(compare_design_estimates(), "not implemented")
  expect_error(compare_design_inquiries(), "not implemented")
  expect_error(compare_design_summaries(), "not implemented")
  expect_error(print_code(), "not implemented")
})

test_that("each compare_* error names compare_diagnoses() as the thing that is implemented", {
  # The point of keeping these is the replacement, not the refusal.
  expect_error(compare_design_code(), "compare_diagnoses")
  expect_error(compare_designs(), "Share the R script")
})

test_that("declare_potential_outcomes() errors and names the model step", {
  expect_error(declare_potential_outcomes(Y ~ 0.2 * Z + U),
               "not a step in DeclareDesign 2.0")
  expect_error(declare_potential_outcomes(Y ~ 0.2 * Z + U), "declare_model")
  expect_error(declare_potential_outcomes(Y ~ 0.2 * Z + U), "potential_outcomes")
  # Without it R reports `could not find function`, which says nothing about
  # the replacement, so the function existing at all is the contract.
  expect_true(is.function(declare_potential_outcomes))
})

test_that("declare_reveal() errors and names the measurement step", {
  expect_error(declare_reveal(Y, Z), "not a step in DeclareDesign 2.0")
  expect_error(declare_reveal(Y, Z), "declare_measurement")
  expect_error(declare_reveal(Y, Z), "reveal_outcomes")
  expect_true(is.function(declare_reveal))
})

test_that("a defunct step errors where 1.x code puts it, inside a design", {
  # `+` evaluates its arguments, so the error arrives at declaration rather
  # than at the first draw. That is where a reader of 1.x code will be.
  expect_error(
    declare_model(N = 10, U = rnorm(N)) +
      declare_potential_outcomes(Y ~ 0.2 * Z + U),
    "not a step in DeclareDesign 2.0"
  )
})

# The advice each defunct error gives has to work ----

test_that("potential outcomes in the model, as the error says to write them", {
  dat <- draw_data(declare_model(N = 4, U = rnorm(N), Z = rep(0:1, 2),
                                 potential_outcomes(Y ~ 0.2 * Z + U)))
  expect_true(all(c("Y_Z_0", "Y_Z_1") %in% names(dat)))
  expect_equal(dat$Y_Z_1 - dat$Y_Z_0, rep(0.2, 4))
})

test_that("named potential outcomes are ordinary model variables, as the error says", {
  dat <- draw_data(declare_model(N = 4, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.2))
  expect_true(all(c("Y_Z_0", "Y_Z_1") %in% names(dat)))
  expect_equal(dat$Y_Z_1 - dat$Y_Z_0, rep(0.2, 4))
})

test_that("reveal_outcomes() in the measurement step, as the error says to write it", {
  dat <- draw_data(
    declare_model(N = 4, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.2) +
      declare_assignment(Z = complete_ra(N)) +
      declare_measurement(Y = reveal_outcomes(Y ~ Z))
  )
  expect_true("Y" %in% names(dat))
  expect_equal(dat$Y, ifelse(dat$Z == 1, dat$Y_Z_1, dat$Y_Z_0))
})

test_that("several assignment variables on the right-hand side, as the error says", {
  dat <- draw_data(
    declare_model(N = 4, U = rnorm(N),
                  potential_outcomes(Y ~ 0.1 * A + 0.2 * B + U,
                                     conditions = list(A = 0:1, B = 0:1))) +
      declare_assignment(A = complete_ra(N), B = complete_ra(N)) +
      declare_measurement(Y = reveal_outcomes(Y ~ A + B))
  )
  expect_true(all(c("Y_A_0_B_0", "Y_A_1_B_1") %in% names(dat)))
  expect_equal(dat$Y, dat$U + 0.1 * dat$A + 0.2 * dat$B)
})
