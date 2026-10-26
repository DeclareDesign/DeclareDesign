# `declare_diagnosands()` and the default set: what a diagnosand expression
# can see, and what happens to one that cannot be computed.

test_that("declare_diagnosands binds alpha inside diagnosand expressions", {
  design <- simple_design(N = 30, ate = 0)
  sims <- simulate_design(design, sims = 30)
  d <- diagnose_simulations(sims, bootstrap_sims = 0,
                            diagnosands = declare_diagnosands(
                              power = mean(p.value <= alpha), alpha = 1))
  expect_equal(get_diagnosands(d)$power, 1)
})

test_that("a diagnosand written as a bare constant computes", {
  # `enquos()` captures a constant with the empty environment, so the
  # tryCatch() wrapper the diagnosands are computed through could not find
  # itself and the failure surfaced as rlang's "bad value" from the traceback
  # rather than as anything a reader could act on.
  design <- declare_model(N = 30, Y_Z_0 = rnorm(N), Y_Z_1 = Y_Z_0 + 0.4) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = randomizr::complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  sims <- simulate_design(design, sims = 5)

  d <- diagnose_design(sims, bootstrap_sims = 0,
                       diagnosands = declare_diagnosands(k = 500))
  expect_true(all(d$diagnosands_df$k == 500))

  d2 <- diagnose_design(sims, bootstrap_sims = 0,
                        diagnosands = declare_diagnosands(
                          bias = mean(estimate - estimand, na.rm = TRUE),
                          k = 500))
  expect_true(all(d2$diagnosands_df$k == 500))
  expect_true(is.numeric(d2$diagnosands_df$bias))
})

test_that("a diagnosand that errors comes back NA rather than aborting", {
  design <- declare_model(N = 30, Y_Z_0 = rnorm(N), Y_Z_1 = Y_Z_0 + 0.4) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = randomizr::complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  sims <- simulate_design(design, sims = 5)
  d <- diagnose_design(sims, bootstrap_sims = 0,
                       diagnosands = declare_diagnosands(boom = stop("no")))
  expect_true(all(is.na(d$diagnosands_df$boom)))
})

test_that("a diagnosands step evaluates its own expressions the way diagnose_design does", {
  # The step is a function and nothing in the package calls it: diagnose_design
  # reads the dots off the step and evaluates them in compute_diagnosands().
  # Asserting the two agree on one table is what would catch them drifting.
  design <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  sims <- simulate_design(design, sims = 6)
  diagnosands <- declare_diagnosands(bias = mean(estimate - estimand),
                                     power = mean(p.value <= alpha))

  direct <- diagnosands(sims)
  expect_equal(direct$diagnosand, c("bias", "power"))

  computed <- DeclareDesign:::compute_diagnosands(sims, diagnosands, character(0))
  expect_equal(direct$value[direct$diagnosand == "bias"], computed$bias)
  expect_equal(direct$value[direct$diagnosand == "power"], computed$power)
})

test_that("select_diagnosands() forwards its subset as an expression, not a quosure", {
  diagnosands <- select_diagnosands("bias", "power", subset = p.value < 0.5)
  subset_quo <- attr(diagnosands, "subset_quo")
  expect_true(rlang::is_quosure(subset_quo))
  expect_false(rlang::is_quosure(rlang::quo_get_expr(subset_quo)))
  expect_equal(rlang::quo_get_expr(subset_quo), quote(p.value < 0.5))
})
