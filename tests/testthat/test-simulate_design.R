# `simulate_design()`: the simulations table, over one design or a list of
# them.
#
# Nested draws are test-draws.R, failed estimators test-estimator-failures.R,
# seeds test-seeds.R, and parallel plans test-capture.R.

d1 <- function() declare_model(N = 6, Y = rnorm(N)) + declare_inquiry(m = mean(Y))
d2 <- function() declare_model(N = 8, Y = rnorm(N)) + declare_inquiry(m = mean(Y))

test_that("simulate_design needs a design", {
  expect_error(simulate_design(sims = 2), "requires at least one `design`")
})

test_that("a bare step simulates as a one-step design", {
  sims <- simulate_design(declare_inquiry(m = 1), sims = 2)
  expect_equal(unique(sims$design), "design_1")
  expect_equal(sims$estimand, c(1, 1))
})

test_that("a list of designs is labelled by the name it was supplied under", {
  # One design in a named list takes the name itself, since there is nothing
  # to tell apart; several take the name as a prefix.
  one <- simulate_design(a = list(d1()), sims = 2)
  expect_equal(unique(one$design), "a")
  two <- simulate_design(a = list(d1(), d2()), sims = 2)
  expect_equal(unique(two$design), c("a_design_1", "a_design_2"))
})

test_that("two lists carrying the same inner name are held apart", {
  sims <- simulate_design(list(x = d1(), y = d2()), list(x = d2()), sims = 2)
  expect_equal(unique(sims$design), c("x", "y", "x.1"))
})

test_that("`add_grouping_variables` names its replacement rather than being ignored", {
  # It fell through the design filter and was silently dropped, so a grouped
  # simulation came back ungrouped while the migration vignette said it
  # errored.
  expect_error(
    simulate_design(d1(), sims = 2, add_grouping_variables = "N"),
    "group the simulations with `group_by\\(\\)`"
  )
})

# Nested draws: the fan-out paths. A step with `draws > 1` splits the steps
# below it, and the steps above it run once, outside the fan.

test_that("a step that is neither DGP, inquiry nor estimator is passed over inside the fan", {
  # `design + declare_diagnosands(...)` leaves a diagnosands step sitting after
  # the fan-out, and the recursion has to walk through it rather than treat it
  # as something to run.
  design <- declare_model(N = 20, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.3) +
    declare_assignment(Z = complete_ra(N), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z") +
    declare_diagnosands(mean_est = mean(estimate))
  sims <- simulate_design(design)
  expect_equal(nrow(sims), 3L)
  expect_equal(sort(sims$assignment_draw), 1:3)
})

test_that("an estimator upstream of the first fan-out runs once, not once per draw", {
  # The population regression does not depend on the assignment, so it belongs
  # outside the fan: 1 row, against 3 for the estimator below the fan.
  design <- declare_model(N = 40, U = rnorm(N), X = rnorm(N), Y_Z_0 = U + X,
                          Y_Z_1 = Y_Z_0 + 0.3) +
    declare_estimator(Y_Z_0 ~ X, .method = lm, term = "X", label = "pop_reg") +
    declare_assignment(Z = complete_ra(N), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ate")
  sims <- simulate_design(design)
  expect_equal(sum(sims$estimator == "pop_reg"), 1L)
  expect_equal(sum(sims$estimator == "ate"), 3L)
  expect_true(is.na(sims$assignment_draw[sims$estimator == "pop_reg"]))
})

test_that("a nested design with nothing to report simulates to an empty table", {
  design <- declare_model(N = 10, U = rnorm(N)) +
    declare_assignment(Z = complete_ra(N), draws = 3)
  expect_equal(nrow(simulate_design(design)), 0L)
})

test_that("a nested estimator with no `inquiry =` still gets the estimand attached", {
  # The inquiry runs in the prefix, so it carries no draw column and the
  # estimator carries no inquiry label: there is nothing to join on, and every
  # estimate takes the one realised estimand.
  design <- declare_model(N = 30, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.3) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")
  sims <- simulate_design(design)
  expect_equal(nrow(sims), 3L)
  expect_equal(length(unique(sims$estimand)), 1L)
})

test_that("a parameter does not overwrite a column the simulation already reports", {
  # A design parameterised by its effect size, with the parameter named
  # `estimand`: the column holds the realised inquiry, which is what diagnosis
  # needs, and the declared value is left out rather than written over it.
  design <- declare_parameters(estimand = 0.3) +
    declare_model(N = 30, U = rnorm(N), tau = estimand + rnorm(N, sd = 0.5),
                  Y_Z_0 = U, Y_Z_1 = U + tau) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")
  sims <- simulate_design(!!!redesign(design, estimand = c(0.3, 0.9)), sims = 3)
  expect_false(any(sims$estimand %in% c(0.3, 0.9)))
  expect_gt(mean(sims$estimand[sims$design == "estimand = 0.9"]),
            mean(sims$estimand[sims$design == "estimand = 0.3"]))
})
