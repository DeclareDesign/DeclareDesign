# Every draw runs on its own L'Ecuyer-CMRG stream whichever plan is active, so
# a seeded run gives one table, not one per plan.

seed_design <- function() {
  declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + 0.2 * Z) +
    declare_inquiry(ATE = 0.2) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")
}

test_that("a seeded sequential run is reproducible", {
  design <- seed_design()
  set.seed(11); a <- simulate_design(design, sims = 6)
  set.seed(11); b <- simulate_design(design, sims = 6)
  expect_equal(a$estimate, b$estimate)
  set.seed(12); c <- simulate_design(design, sims = 6)
  expect_false(identical(a$estimate, c$estimate))
})

test_that("sequential and multisession plans give the same numbers for the same seed", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  design <- seed_design()
  set.seed(11); seq_run <- simulate_design(design, sims = 6)
  old <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old), add = TRUE)
  set.seed(11); par_run <- simulate_design(design, sims = 6)
  expect_equal(seq_run$estimate, par_run$estimate)

  nested <- declare_model(N = 40, U = rnorm(N), draws = 3) +
    declare_inquiry(mu = mean(U)) +
    declare_measurement(Y = U + rnorm(N), draws = 2) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu")
  set.seed(13); par_nested <- simulate_design(nested)
  future::plan(old)
  set.seed(13); seq_nested <- simulate_design(nested)
  expect_equal(seq_nested$estimate, par_nested$estimate)
})

test_that("the caller's generator is left as it was found, one draw on", {
  design <- seed_design()
  kind <- RNGkind()
  set.seed(5); x_before <- runif(3)
  set.seed(5); invisible(simulate_design(design, sims = 3)); x_after <- runif(3)
  expect_equal(RNGkind(), kind)
  expect_false(identical(x_before, x_after))
  set.seed(5); invisible(sample.int(.Machine$integer.max, 1L)); x_one_on <- runif(3)
  expect_equal(x_after, x_one_on)
})
