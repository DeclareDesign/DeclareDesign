test_that("draws attribute is stored on design steps", {
  s <- declare_model(N = 10, Y = rnorm(N), draws = 5)
  expect_equal(attr(s, "draws"), 5L)
  s2 <- declare_inquiry(mu = mean(Y), draws = 3)
  expect_equal(attr(s2, "draws"), 3L)
  s3 <- declare_estimator(Y ~ Z, .method = lm, term = "Z", draws = 7)
  expect_equal(attr(s3, "draws"), 7L)
  s4 <- declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 4)
  expect_equal(attr(s4, "draws"), 4L)
  s5 <- declare_sampling(S = sample(rep(0:1, length.out = N)), draws = 2)
  expect_equal(attr(s5, "draws"), 2L)
  s6 <- declare_measurement(Y = Y * 2, draws = 9)
  expect_equal(attr(s6, "draws"), 9L)
  s7 <- declare_test(Y ~ Z, .method = lm, draws = 11)
  expect_equal(attr(s7, "draws"), 11L)
})

test_that("default draws is 1", {
  s <- declare_model(N = 10, Y = rnorm(N))
  expect_equal(attr(s, "draws"), 1L)
})

test_that("design_has_nested_draws returns TRUE when any step has draws > 1", {
  d1 <- declare_model(N = 10, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))
  expect_false(design_has_nested_draws(d1))

  d2 <- declare_model(N = 10, Y = rnorm(N), draws = 5) +
    declare_inquiry(mu = mean(Y))
  expect_true(design_has_nested_draws(d2))
})

test_that("nested simulation produces world_sims * design_sims paths", {
  design <-
    declare_model(N = 10, U = rnorm(N), Y_Z_1 = U + 0.3, Y_Z_0 = U,
                  draws = 5) +
    declare_inquiry(mu = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu",
                      label = "ols")
  sim <- simulate_design(design)
  expect_equal(max(sim$sim_ID), 5L * 3L)
})

test_that("nested simulation adds <label>_draw columns", {
  design <-
    declare_model(N = 10, U = rnorm(N), Y_Z_1 = U + 0.3, Y_Z_0 = U,
                  draws = 5) +
    declare_inquiry(mu = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu",
                      label = "ols")
  sim <- simulate_design(design)
  expect_true("model_draw" %in% names(sim))
  expect_true("assignment_draw" %in% names(sim))
  expect_equal(length(unique(sim$model_draw)), 5L)
  expect_equal(length(unique(sim$assignment_draw)), 3L)
})

test_that("warn when sims and draws both specified", {
  design <-
    declare_model(N = 10, U = rnorm(N), Y_Z_1 = U + 0.3, Y_Z_0 = U,
                  draws = 5) +
    declare_inquiry(mu = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu",
                      label = "ols")
  expect_warning(simulate_design(design, sims = 4), "flat simulation")
})

test_that("diagnose_design uses nested draws when sims is NULL", {
  design <-
    declare_model(N = 20, U = rnorm(N), Y_Z_1 = U + .3, Y_Z_0 = U,
                  draws = 4) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  diag <- diagnose_design(design, bootstrap_sims = 0)
  expect_equal(max(diag$simulations_df$sim_ID), 4L * 3L)
  expect_false(is.null(diag$variance_decomposition))
})

test_that("variance decomposition has var_total, var_design, var_world, fracs", {
  design <-
    declare_model(N = 20, U = rnorm(N), Y_Z_1 = U + .3, Y_Z_0 = U,
                  draws = 4) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N)), draws = 3) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  diag <- diagnose_design(design, bootstrap_sims = 0)
  vd <- diag$variance_decomposition
  expect_true(all(c("var_total", "var_design", "var_world",
                    "frac_design", "frac_world") %in% names(vd)))
  expect_true(all(vd$frac_design + vd$frac_world > 0, na.rm = TRUE))
})

test_that("only-model draws works (no assignment fan-out)", {
  design <-
    declare_model(N = 30, U = rnorm(N), Y_Z_1 = U + .3, Y_Z_0 = U,
                  draws = 4) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  sim <- simulate_design(design)
  expect_equal(max(sim$sim_ID), 4L)
  expect_true("model_draw" %in% names(sim))
  expect_false("assignment_draw" %in% names(sim))
})

test_that("flat diagnose_design still works unchanged", {
  design <- declare_model(N = 20, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                      inquiry = "mu", label = "ols")
  diag <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_null(diag$variance_decomposition)
})

test_that("three-level nesting works", {
  design <-
    declare_model(N = 100, U = rnorm(N), Y_Z_1 = U + .3, Y_Z_0 = U,
                  draws = 3) +
    declare_sampling(S = c(rep(1, 50), rep(0, 50)), draws = 2) +
    declare_assignment(Z = sample(rep(0:1, length.out = length(U))),
                       draws = 4) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
  sim <- simulate_design(design)
  expect_equal(max(sim$sim_ID), 3L * 2L * 4L)
})
