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
