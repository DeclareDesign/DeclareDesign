test_that("diagnose_design accepts custom diagnosands via set_diagnosands", {
  design <- simple_design(N = 30) |>
    set_diagnosands(declare_diagnosands(
      mean_estimate = mean(estimate, na.rm = TRUE)
    ))
  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_true("mean_estimate" %in% names(get_diagnosands(d)))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("select_diagnosands subsets diagnosands", {
  diags <- default_diagnosands()
  trimmed <- select_diagnosands(diags, "bias", "rmse")
  expect_equal(names(attr(trimmed, "dots")), c("bias", "rmse"))
})

test_that("select_diagnosands builds a set from the library, as in DeclareDesign", {
  diags <- select_diagnosands("sd_estimate", "mean_se")
  expect_equal(names(attr(diags, "dots")), c("sd_estimate", "mean_se"))
  d <- diagnose_design(simple_design(N = 30), diagnosands = diags, sims = 10,
                       bootstrap_sims = 0)
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("design", "inquiry", "estimator", "outcome", "term",
                         "n_sims")),
               c("sd_estimate", "mean_se"))
})

test_that("select_diagnosands reaches diagnosands outside the default set", {
  extra <- c("type_s_rate", "exaggeration_ratio", "var_estimate",
             "mean_var_hat", "prop_pos_sig", "mean_ci_length", "mean_estimand")
  diags <- rlang::inject(select_diagnosands(!!!extra))
  expect_equal(names(attr(diags, "dots")), extra)
})

test_that("select_diagnosands names an unknown diagnosand rather than ignoring it", {
  expect_error(select_diagnosands("bais"), "Unknown diagnosand")
  expect_error(select_diagnosands(default_diagnosands(), "bais"),
               "not in this set")
})

test_that("select_diagnosands passes alpha through to power", {
  sims <- simulate_design(simple_design(N = 30, ate = 0), sims = 30)
  loose <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = select_diagnosands("power", alpha = 1))
  strict <- diagnose_simulations(sims, bootstrap_sims = 0,
                                 diagnosands = select_diagnosands("power", alpha = 0))
  expect_equal(get_diagnosands(loose)$power, 1)
  expect_equal(get_diagnosands(strict)$power, 0)
})

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

test_that("declare_diagnosands binds alpha inside diagnosand expressions", {
  design <- simple_design(N = 30, ate = 0)
  sims <- simulate_design(design, sims = 30)
  d <- diagnose_simulations(sims, bootstrap_sims = 0,
                            diagnosands = declare_diagnosands(
                              power = mean(p.value <= alpha), alpha = 1))
  expect_equal(get_diagnosands(d)$power, 1)
})

test_that("diagnose_design over multiple designs adds a design column", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  d <- diagnose_design(!!!fam, sims = 5, bootstrap_sims = 0)
  diag <- get_diagnosands(d)
  expect_true("design" %in% names(diag))
  expect_equal(nrow(diag), 2L)
})

test_that("modify_design (insert/delete/replace) still works after deprecation", {
  design <- declare_model(N = 30, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))

  inserted <- suppressWarnings(insert_step(design, declare_measurement(Y2 = Y * 2), after = "model"))
  expect_equal(length(inserted), 3L)

  deleted <- suppressWarnings(delete_step(design, "mu"))
  expect_equal(length(deleted), 1L)

  replaced <- suppressWarnings(replace_step(design, "mu", declare_inquiry(med = median(Y))))
  expect_equal(length(replaced), 2L)
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

test_that("legacy `model =` is read as `.method` with a deprecation warning", {
  skip_if_not_installed("estimatr")
  expect_warning(
    step <- declare_estimator(Y ~ Z, model = estimatr::lm_robust, term = "Z",
                              label = "ols"),
    "deprecated"
  )
  expect_identical(attr(step, "method_name"), "lm_robust")
  design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20),
                          Y = U + 0.5 * Z) + step
  est <- draw_estimates(design)
  expect_equal(nrow(est), 1L)
  expect_false("model" %in% names(est))
})

test_that("select_diagnosands refuses library arguments when subsetting a set", {
  diags <- declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
  expect_error(select_diagnosands(diags, "power", alpha = 0.5),
               "cannot be applied to a diagnosands set that already exists")
  expect_error(select_diagnosands(diags, "power", subset = p.value < 1),
               "cannot be applied")
  expect_no_error(select_diagnosands(diags, "power"))
})

test_that("select_diagnosands says so when the step is not a diagnosands set", {
  expect_error(select_diagnosands(declare_model(N = 5, Y = rnorm(N)), "bias"),
               "this is a model step")
})

test_that("reshape_diagnosis gives columns their display names", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  out <- reshape_diagnosis(d)
  expect_true(all(c("Inquiry", "Estimator", "Term", "N Sims",
                    "Mean Estimand", "SD Estimate", "RMSE", "Power") %in%
                    names(out)))
  expect_equal(nrow(out), 1L)
  expect_type(out[["Bias"]], "character")
})

test_that("reshape_diagnosis puts bootstrap SEs in parentheses below", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 10)
  out <- reshape_diagnosis(d)
  expect_equal(nrow(out), 2L)
  expect_match(out[["Bias"]][2], "^\\(.*\\)$")
  expect_equal(out[["Inquiry"]][2], "")
})

test_that("reshape_diagnosis leaves choosing columns to select()", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  expect_error(reshape_diagnosis(d, select = c("Bias", "Power")), "unused")
  expect_equal(names(reshape_diagnosis(d) |> dplyr::select(Term, Bias, Power)),
               c("Term", "Bias", "Power"))
})

test_that("reshape_diagnosis is DeclareDesign's name for format()", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 10)
  expect_identical(reshape_diagnosis(d), format(d))
  expect_identical(reshape_diagnosis(d, digits = 4), format(d, digits = 4))
  capture.output(out <- print(d))
  expect_identical(out, format(d))
})

test_that("format(diagnosis) rounds to digits", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  expect_match(format(d, digits = 4)[["Bias"]], "^-?[0-9]+\\.[0-9]{4}$")
  expect_match(format(d, digits = 1)[["Bias"]], "^-?[0-9]+\\.[0-9]$")
})

test_that("reshape_diagnosis leaves redesign parameter names alone", {
  designs <- redesign(simple_design(N = 30), ate = c(0.1, 0.5))
  d <- diagnose_design(designs, sims = 5, bootstrap_sims = 0)
  out <- reshape_diagnosis(d)
  expect_true("ate" %in% names(out))
})

test_that("tidy(diagnosis) carries the bootstrap interval", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 20)
  td <- tidy(d)
  expect_true(all(c("diagnosand", "estimate", "std.error", "conf.low",
                    "conf.high") %in% names(td)))
  expect_false(any(grepl("^se\\(", names(td))))
  bias <- td[td$diagnosand == "bias", ]
  expect_true(bias$conf.low <= bias$estimate)
  expect_true(bias$estimate <= bias$conf.high)
  expect_false("conf.low" %in% names(tidy(d, conf.int = FALSE)))
})

test_that("tidy(diagnosis) works without a bootstrap", {
  d <- diagnose_design(simple_design(N = 30), sims = 10, bootstrap_sims = 0)
  td <- tidy(d)
  expect_true(all(c("diagnosand", "estimate") %in% names(td)))
  expect_false("std.error" %in% names(td))
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

test_that("format() finds the parameter names on a DeclareDesign diagnosis", {
  # A diagnosis produced by DeclareDesign and read back in carries its
  # redesign parameters as `parameters_df`, not as an attribute of the
  # simulations table. Without this the parameter column gets title-cased
  # and `b` becomes `B`, which is what broke the course's saved diagnoses.
  d <- diagnose_design(redesign(simple_design(N = 30), ate = c(0.1, 0.5)),
                       sims = 5, bootstrap_sims = 0)
  attr(d$simulations_df, "parameter_names") <- NULL
  d$parameters_df <- data.frame(design = c("design_1", "design_2"),
                                ate = c(0.1, 0.5))
  expect_true("ate" %in% names(format(d)))
  expect_false("Ate" %in% names(format(d)))
})

test_that("a design carrying DeclareDesign diagnosands falls back to the defaults", {
  # Found by running DesignLibrary: two of its designers call
  # DeclareDesign's set_diagnosands(), and the object that leaves on the
  # design has the same class, step_type and causal_type as ours, so only the
  # dots tell them apart. We used to read it and die inside quo_get_expr().
  design <- simple_design(N = 30)
  foreign <- structure(
    function(data) data,
    dots = list(data = quote(data), bias = rlang::quo(mean(estimate - estimand))),
    step_type = "diagnosand", causal_type = "diagnosands",
    class = c("design_step", "dd", "function")
  )
  attr(design, "diagnosands") <- foreign

  expect_warning(d <- diagnose_design(design, sims = 5, bootstrap_sims = 0),
                 "cannot be read here")
  expect_true(all(c("bias", "rmse", "power", "coverage") %in%
                    names(get_diagnosands(d))))
  expect_error(
    diagnose_design(simple_design(N = 30), diagnosands = foreign, sims = 5),
    "not a declare_diagnosands\\(\\) object from this package"
  )
})

test_that("our own set_diagnosands object is still read", {
  design <- simple_design(N = 30) |>
    set_diagnosands(declare_diagnosands(mean_estimate = mean(estimate)))
  expect_no_warning(d <- diagnose_design(design, sims = 5, bootstrap_sims = 0))
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("design", "inquiry", "estimator", "outcome", "term",
                         "n_sims")),
               "mean_estimate")
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
