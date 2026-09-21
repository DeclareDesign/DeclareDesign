# print() shows the calls that declared the steps; summary() runs the design
# once and says what each step did.

ps_design <- function() {
  declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(S = sample(rep(0:1, 20))) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
}

test_that("print() shows each step as the call that declared it", {
  out <- capture.output(print(ps_design()))
  expect_match(out[1], "^Research design with 6 steps$")
  expect_true(any(grepl("^Step 3 \\(sampling\\): declare_sampling\\(S = sample\\(rep\\(0:1, 20\\)\\)\\)$", out)))
  expect_true(any(grepl("^Step 6 \\(estimator\\): declare_estimator\\(Y ~ Z, .method = lm", out)))
})

test_that("summary() accounts for what each step did on one run", {
  s <- summary(ps_design())
  expect_s3_class(s, "summary.design")
  expect_true(s$ran)
  expect_equal(s$steps$one_run[1], "N = 40 rows; adds ID, U, Y_Z_0, Y_Z_1")
  expect_equal(s$steps$one_run[2], "ATE = 0.5")
  expect_equal(s$steps$one_run[3], "keeps 20 of 40 rows; adds S")
  expect_equal(s$steps$one_run[4], "adds Z")
  expect_equal(s$steps$one_run[5], "adds Y")
  expect_match(s$steps$one_run[6], "^Z = .* \\(std.error .*\\)$")
  expect_equal(nrow(s$data), 20L)
  expect_equal(s$inquiries$estimand, 0.5)
  expect_equal(s$estimates$estimand, 0.5)
  out <- capture.output(print(s))
  expect_true(any(grepl("^  keeps 20 of 40 rows; adds S$", out)))
  expect_true(any(grepl("^One run of the design:$", out)))
})

test_that("summary(run = FALSE) does not run the design", {
  ran <- FALSE
  design <- declare_model(N = 10, Y = rnorm(N)) +
    declare_estimator(handler = function(data) {
      ran <<- TRUE
      data.frame(estimate = 1)
    })
  s <- summary(design, run = FALSE)
  expect_false(ran)
  expect_false(s$ran)
  expect_null(s$estimates)
  expect_false("one_run" %in% names(s$steps))
  out <- capture.output(print(s))
  expect_false(any(grepl("One run", out)))
})

test_that("summary() reports a changed column and a failed estimator", {
  design <- declare_model(N = 10, Y = 1) +
    declare_measurement(Y = Y + 1) +
    declare_estimator(handler = function(data) stop("no fit today"),
                      label = "broken")
  s <- summary(design)
  expect_equal(s$steps$one_run[2], "changes Y")
  expect_match(s$steps$one_run[3], "^failed: no fit today")
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
