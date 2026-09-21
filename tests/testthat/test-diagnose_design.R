# `diagnose_design()` itself: what it computes, over how many designs, which
# simulations count, and what it says about a match it could not make.
#
# Diagnosand declaration is test-declare_diagnosands.R, the diagnosand library
# and `set_diagnosands()` are test-set_diagnosands.R, and printing a diagnosis
# is test-print_methods.R.

test_that("a diagnosands subset restricts which simulations count", {
  sims <- simulate_design(simple_design(N = 30), sims = 30)
  all_sims <- diagnose_simulations(sims, bootstrap_sims = 0,
                                   diagnosands = declare_diagnosands(
                                     n = dplyr::n()))
  significant <- diagnose_simulations(sims, bootstrap_sims = 0,
                                      diagnosands = declare_diagnosands(
                                        n = dplyr::n(), subset = p.value <= 0.05))
  expect_equal(get_diagnosands(all_sims)$n, 30L)
  expect_lt(get_diagnosands(significant)$n, 30L)
})

test_that("diagnose_design over multiple designs adds a design column", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  d <- diagnose_design(!!!fam, sims = 5, bootstrap_sims = 0)
  diag <- get_diagnosands(d)
  expect_true("design" %in% names(diag))
  expect_equal(nrow(diag), 2L)
})

test_that("bootstrap tolerates diagnosands that cannot be computed", {
  # Regression test. The point estimate wrapped each diagnosand in a tryCatch
  # and the bootstrap did not, so a design with no inquiry diagnosed at
  # bootstrap_sims = 0 and errored at the default of 100.
  design <- declare_model(N = 60, Y = rbinom(N, 1, 0.55)) +
    declare_test(Y ~ 1, .method = lm, term = "(Intercept)", label = "t")
  diag <- diagnose_design(design, sims = 10, bootstrap_sims = 10)
  diag_df <- get_diagnosands(diag)
  expect_true(all(is.na(diag_df$bias)))
  expect_true(is.finite(diag_df$mean_estimate))
  expect_true("se(mean_estimate)" %in% names(diag_df))
})

test_that("designs supplied in a list keep the list's own names", {
  designs <- list(dum = simple_design(N = 30), dee = simple_design(N = 30))
  d <- diagnose_design(designs, sims = 5, bootstrap_sims = 0)
  expect_setequal(get_diagnosands(d)$design, c("dum", "dee"))
  expect_setequal(reshape_diagnosis(d)[["Design"]], c("dum", "dee"))
  sims <- simulate_design(designs, sims = 3)
  expect_setequal(sims$design, c("dum", "dee"))
})

test_that("designs supplied as bare symbols are named for the symbol", {
  dum <- simple_design(N = 30)
  dee <- simple_design(N = 30)
  sims <- simulate_design(dum, dee, sims = 3)
  expect_setequal(sims$design, c("dum", "dee"))
})

test_that("the diagnosis reports a match that did not go on inquiry", {
  unlabelled <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")
  d <- diagnose_design(unlabelled, sims = 5, bootstrap_sims = 0)
  expect_equal(d$matched_on, "sim_ID")
  expect_output(print(d), "no estimator named an inquiry")
  expect_output(summary(d), "no estimator named an inquiry")
})

test_that("the diagnosis says nothing when the match went on inquiry", {
  d <- diagnose_design(simple_design(N = 30), sims = 5, bootstrap_sims = 0)
  expect_setequal(d$matched_on, c("sim_ID", "inquiry"))
  expect_false(any(grepl("matched to inquiries",
                         capture.output(print(d)))))
})

test_that("the diagnosis names the extra columns a match went on", {
  design <- declare_model(N = 60, g = rep(c("a", "b", "c"), 20),
                          U = rnorm(N), Y = U + as.numeric(g == "b")) +
    declare_inquiry(handler = function(data) {
      data |>
        dplyr::group_by(g) |>
        dplyr::summarize(inquiry = "group_mean", estimand = mean(Y),
                         .groups = "drop")
    }) +
    declare_estimator(handler = function(data) {
      data |>
        dplyr::group_by(g) |>
        dplyr::summarize(term = "mean", estimate = mean(Y), .groups = "drop") |>
        dplyr::mutate(inquiry = "group_mean", estimator = "means")
    })
  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_output(print(d), "matched to inquiries on inquiry, g")
})

test_that("a `design` column is emitted for one design as well as for several", {
  # DeclareDesign 1.1.1 always emits it, and book-era code groups by it. Gating
  # it on `length(designs) > 1` made the break conditional on the number of
  # designs: 1.x code worked on a redesigned list and failed on the single
  # design it was written for.
  one <- simple_design(N = 30)
  expect_true("design" %in% names(simulate_design(one, sims = 3)))

  d <- diagnose_design(one, sims = 3, bootstrap_sims = 0)
  expect_true("design" %in% names(get_simulations(d)))
  expect_true("design" %in% names(get_diagnosands(d)))

  # the label is the name the design was supplied under, as in 1.1.1
  expect_equal(unique(as.character(simulate_design(one, sims = 2)$design)),
               "one")
  expect_equal(unique(as.character(simulate_design(mine = one, sims = 2)$design)),
               "mine")
})

test_that("a 1.x argument to simulate or diagnose errors instead of vanishing", {
  # `make_groups = vars(N)` used to return an ungrouped table and
  # `future.seed = TRUE` used to do nothing, both without a message.
  design <- simple_design(N = 20)
  expect_error(diagnose_design(design, sims = 2, bootstrap_sims = 0,
                               make_groups = vars(N)),
               "make_groups")
  expect_error(simulate_design(design, sims = 2, future.seed = TRUE),
               "future.seed")
  expect_error(simulate_design(design, sims = 2, data.frame(x = 1)),
               "not a design")
})

test_that("an estimator naming an inquiry no step produced warns once", {
  design <- declare_model(N = 20, U = rnorm(N), Y = U) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                      inquiry = "mew", label = "ols")
  rlang::reset_warning_verbosity("dd_unmatched_inquiry_mew")
  expect_warning(out <- run_design(design), "`ols` names inquiry `mew`")
  expect_true(is.na(out$estimand[out$estimator %in% "ols"]))
})

test_that("an inquiry no estimator targets keeps its own diagnosis row, as in 1.x", {
  design <- declare_model(N = 20, U = rnorm(N), Y = U) +
    declare_inquiry(mu = mean(Y)) +
    declare_inquiry(sigma = sd(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                      inquiry = "mu", label = "ols")
  diag <- diagnose_design(design, sims = 3, bootstrap_sims = 0)$diagnosands_df
  expect_setequal(diag$inquiry, c("mu", "sigma"))
  sigma_row <- diag[diag$inquiry == "sigma", ]
  expect_true(is.na(sigma_row$estimator))
  expect_false(is.na(sigma_row$mean_estimand))
})

# diagnose_design() as the unified entry point ----
#
# Moved from test-autolabel.R, which is a file about estimator labelling.
test_that("simulate_design |> diagnose_design() works (df piped in)", {
  design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu")
  diag <- design |> simulate_design(sims = 5) |>
    diagnose_design(bootstrap_sims = 0)
  expect_s3_class(diag, "diagnosis")
})

test_that("group_by() upstream of diagnose_simulations adds groups", {
  design <- declare_model(N = 50, Y = rnorm(N), Z = rep(0:1, 25)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu")
  diag <- design |>
    simulate_design(sims = 10) |>
    dplyr::mutate(big = estimate > 0) |>
    dplyr::group_by(big) |>
    diagnose_simulations(bootstrap_sims = 0)
  expect_true("big" %in% names(diag$diagnosands_df))
  expect_equal(nrow(diag$diagnosands_df), 2L)
})

test_that("group_by |> diagnose_design() works end-to-end", {
  design <- declare_model(N = 50, Y = rnorm(N), Z = rep(0:1, 25)) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "mu")
  diag <- design |>
    simulate_design(sims = 10) |>
    dplyr::mutate(sig = p.value < 0.5) |>
    dplyr::group_by(sig) |>
    diagnose_design(bootstrap_sims = 0)
  expect_s3_class(diag, "diagnosis")
  expect_true("sig" %in% names(diag$diagnosands_df))
})
