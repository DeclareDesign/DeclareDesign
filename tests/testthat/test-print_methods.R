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

test_that("print() on a step names it and its type", {
  out <- capture.output(print(declare_model(N = 10, Y = rnorm(N))))
  expect_equal(out, "<design_step: model [model]>")
  expect_s3_class(declare_model(N = 10), "design_step")
})

test_that("a step with no recorded call falls back to its verb", {
  step <- declare_sampling(S = 1)
  attr(step, "call") <- NULL
  expect_equal(DeclareDesign:::format_step_call(step), "declare_sampling(...)")
})

test_that("print() lists the notes a design takes when it runs", {
  design <- declare_model(N = 10, Y = rnorm(N)) + declare_notes(tallest = max(Y))
  out <- capture.output(print(design))
  expect_true(any(grepl("^Notes the design takes when it runs", out)))
  expect_true(any(grepl("tallest", out) & grepl("max\\(Y\\)", out)))
})

test_that("summary() reports the value a note took, and print() lists it", {
  design <- declare_model(N = 10, Y = as.numeric(1:10)) + declare_notes(tallest = max(Y))
  s <- summary(design)
  expect_equal(s$steps$one_run[2], "tallest = 10")
  out <- capture.output(print(s))
  expect_true(any(grepl("^  tallest = 10$", out)))
  expect_true(any(grepl("^Notes the design takes when it runs", out)))
})

test_that("summary() of an inquiry-only design prints the inquiries", {
  design <- declare_model(N = 10, Y_Z_0 = 0, Y_Z_1 = 1) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  s <- summary(design)
  expect_equal(names(s$estimates), c("inquiry", "estimand"))
  out <- capture.output(print(s))
  expect_true(any(grepl("^One run of the design:$", out)))
  expect_true(any(grepl("ATE", out)))
})

test_that("summary() says which columns a step drops and when it changes nothing", {
  design <- declare_model(N = 10, Y = 1, W = 2) +
    declare_step(dplyr::select, -W) +
    declare_step(dplyr::mutate, Y = Y)
  s <- summary(design)
  expect_equal(s$steps$one_run[2], "drops W")
  expect_equal(s$steps$one_run[3], "leaves the data as it was")
})

test_that("summary() leaves a step that does not run on a draw unaccounted for", {
  design <- declare_parameters(N = 10) +
    declare_model(N = N, Y = rnorm(N))
  s <- summary(design)
  expect_true(is.na(s$steps$one_run[1]))
  expect_equal(s$steps$one_run[2], "N = 10 rows; adds ID, Y")
})

test_that("summary() describes an inquiry or estimator that produced no rows", {
  design <- declare_model(N = 5, Y = 1) +
    declare_inquiry(handler = function(data) {
      data.frame(inquiry = character(0), estimand = numeric(0))
    }) +
    declare_estimator(handler = function(data) data.frame(estimate = numeric(0)),
                      label = "empty")
  s <- summary(design)
  expect_equal(s$steps$one_run[2], "no estimand")
  expect_equal(s$steps$one_run[3], "no estimate")
})

test_that("summary() counts the rows of an estimator table with no estimate column", {
  design <- declare_model(N = 5, Y = 1) +
    declare_estimator(handler = function(data) data.frame(term = c("a", "b"), foo = 1:2),
                      label = "odd")
  expect_equal(summary(design)$steps$one_run[2], "2 rows")
})

test_that("the step accounting guards hold for tables it cannot read", {
  expect_equal(DeclareDesign:::describe_data_change(NULL, "not a frame"),
               "returned no data frame")
  expect_equal(DeclareDesign:::describe_inquiry_rows(data.frame(foo = 1:3)),
               "3 rows")
  expect_equal(DeclareDesign:::describe_inquiry_rows(data.frame(foo = 1)),
               "1 row")
})

test_that("print(diagnosis) reports the nesting and the variance decomposition", {
  design <- declare_model(N = 20, U = rnorm(N), draws = 2) +
    declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.3) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  diagnosis <- diagnose_design(design, bootstrap_sims = 0)
  expect_false(is.null(diagnosis$variance_decomposition))
  out <- capture.output(print(diagnosis))
  expect_true(any(grepl("simulations \\[nested: model\\]", out)))
  expect_true(any(grepl("^Variance decomposition:$", out)))
  expect_false(any(grepl("draw_levels", out)))
})

test_that("print(diagnosis) says nothing about matching when there is nothing to match", {
  design <- declare_model(N = 20, Y_Z_0 = 0, Y_Z_1 = 1) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  diagnosis <- diagnose_design(design, sims = 3, bootstrap_sims = 0)
  expect_null(diagnosis$matched_on)
  out <- capture.output(print(diagnosis))
  expect_false(any(grepl("matched to inquiries", out)))
})

test_that("format() refuses anything that is not a diagnosis", {
  expect_error(DeclareDesign:::format.diagnosis(data.frame(a = 1)),
               "must be a diagnosis object")
})

test_that("tidy() of a diagnosis with no diagnosands returns the table it has", {
  diagnosis <- structure(
    list(diagnosands_df = tibble::tibble(design = "d", n_sims = 3L),
         diagnosand_names = character(0)),
    class = "diagnosis")
  expect_equal(tidy(diagnosis), tibble::tibble(design = "d", n_sims = 3L))
})

test_that("a bootstrap interval is NA when any replicate is NA", {
  expect_true(is.na(DeclareDesign:::quantile_na(c(1, NA, 3), 0.5)))
  expect_equal(DeclareDesign:::quantile_na(c(1, 2, 3), 0.5), 2)
})
