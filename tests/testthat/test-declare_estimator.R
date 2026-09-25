# `declare_estimator()`, `declare_test()` and `label_estimator()`: how a
# method is called, which terms come back, and which inquiry they attach to.
#
# Estimator labels are test-autolabel.R, and an estimator that fails on a draw
# is test-estimator-failures.R.

test_that("declare_estimator returns a tidy table carrying the estimator label", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
  est <- draw_estimates(design)
  expect_true("estimator" %in% names(est))
  expect_equal(est$estimator, "ols")
  expect_equal(est$term, "Z")
})

test_that("an estimator is joined to the inquiry it names", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(ATE = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
  est <- draw_estimates(design)
  expect_true("estimand" %in% names(est))
  expect_equal(est$inquiry, "ATE")
})

test_that("a term filter restricts the rows returned", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), X = rnorm(N),
                          Y = Z + X + rnorm(N)) +
    declare_estimator(Y ~ Z + X, .method = lm, term = "Z", label = "ols")
  est <- draw_estimates(design)
  expect_equal(nrow(est), 1L)
  expect_equal(est$term, "Z")
})

test_that("label_estimator wraps a custom function", {
  my_est <- label_estimator(
    function(data, ...) lm(Y ~ Z, data = data),
    label = "lm", inquiry = "ATE", term = "Z"
  )
  df <- data.frame(Y = rnorm(20), Z = rep(0:1, 10))
  out <- my_est(df)
  expect_equal(out$estimator, "lm")
  expect_equal(out$inquiry, "ATE")
  expect_equal(out$term, "Z")
})

test_that("declare_test does not add an inquiry column", {
  design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_test(Y ~ Z, .method = lm, term = "Z", label = "diff")
  est <- draw_estimates(design)
  expect_false("inquiry" %in% names(est))
  expect_equal(est$estimator, "diff")
})

test_that("an unset term reports the first non-intercept term only", {
  # The 1.x contract, and what Macartan's designs are written against:
  # `declare_estimator(Y ~ Z + X)` is one estimate of one inquiry, and the
  # covariate X is not a second row of it. The rewrite returned every
  # non-intercept term here, so a diagnosis of `ATE` grew an X row.
  design <- declare_model(N = 30, Z = rnorm(N), X = rnorm(N), Y = rnorm(N)) +
    declare_inquiry(ATE = 0) +
    declare_estimator(Y ~ Z + X)
  est <- draw_estimates(design)
  expect_equal(nrow(est), 1L)
  expect_equal(est$term, "Z")
  expect_equal(est$inquiry, "ATE")
  diag <- diagnose_design(design, sims = 2, bootstrap_sims = FALSE)
  expect_equal(diag$diagnosands_df$term, "Z")
  # `term = FALSE` is the same thing spelled out.
  design_false <- declare_model(N = 30, Z = rnorm(N), X = rnorm(N),
                                Y = rnorm(N)) +
    declare_estimator(Y ~ Z + X, .method = lm, term = FALSE)
  expect_equal(draw_estimates(design_false)$term, "Z")
})

test_that("an unset term on an intercept-only fit keeps the intercept", {
  design <- declare_model(N = 30, Y = rnorm(N)) +
    declare_estimator(Y ~ 1, .method = lm)
  est <- draw_estimates(design)
  expect_equal(est$term, "(Intercept)")
})

test_that("a named term the fit does not produce is an error, not a shorter table", {
  # 1.x stopped here; the rewrite dropped the unmatched name silently, so a
  # typo in `term` cost a row rather than raising anything. The estimator
  # failure machinery records it per draw.
  design <- declare_model(N = 30, Z = rnorm(N), Y = rnorm(N)) +
    declare_estimator(Y ~ Z, .method = lm, term = c("Z", "W"))
  expect_error(draw_estimates(design), "W")
  sims <- suppressWarnings(simulate_design(design, sims = 2))
  expect_true(all(sims$error))
  expect_match(sims$error_message, "W")
})

test_that("term = TRUE returns all model rows including (Intercept)", {
  d <- declare_model(N = 50, X = rnorm(N), Y = rnorm(N) + X) +
    declare_estimator(Y ~ X, .method = lm, term = TRUE, label = "ols")
  est <- draw_estimates(d)
  expect_true(all(c("(Intercept)", "X") %in% est$term))
  expect_equal(nrow(est), 2L)
})

test_that("term and inquiry vectors stay aligned in user-supplied order", {
  d <- declare_model(N = 40, X1 = rnorm(N), X2 = rnorm(N),
                     Y = X1 - X2 + rnorm(N)) +
    declare_inquiry(x1 = 1, x2 = -1, interaction = 0) +
    declare_estimator(Y ~ X1 * X2, .method = lm,
                      term = c("X1:X2", "X1", "X2"),
                      inquiry = c("interaction", "x1", "x2"),
                      label = "ols")
  ret <- run_design(d)
  expect_equal(ret$term, c("X1:X2", "X1", "X2"))
  expect_equal(ret$inquiry, c("interaction", "x1", "x2"))
})

test_that("a single estimate row replicates across multiple inquiries", {
  d <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(pate = 1) +
    declare_inquiry(sate = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z",
                      inquiry = c("pate", "sate"), label = "ols")
  e <- draw_estimates(d)
  expect_equal(nrow(e), 2L)
  expect_equal(sort(e$inquiry), c("pate", "sate"))
})

test_that("passing a design_step as inquiry errors with a helpful message", {
  pate <- declare_inquiry(pate = 1)
  expect_error(
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = pate),
    "string, not a step object"
  )
})

test_that("inquiry as a string links estimator to estimand correctly", {
  d <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
    declare_inquiry(pate = 1) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z",
                      inquiry = "pate", label = "ols")
  e <- draw_estimates(d)
  expect_equal(e$inquiry, "pate")
})

test_that("a method receives its arguments as written, not as values", {
  # The rule: `declare_estimator(Y, X, .method = f)` hands `f` the names, so a
  # method that resolves column names itself (pull(data, {{y}}), lm_robust's
  # clusters and weights) can do so. DeclareDesign does the same.
  peek <- function(data, y, x, ...) {
    tibble::tibble(term = "peek", estimate = 0,
                   y_got = rlang::as_label(rlang::enexpr(y)),
                   x_got = rlang::as_label(rlang::enexpr(x)))
  }
  design <- declare_model(N = 10, X = rnorm(N), Y = X) +
    declare_estimator(Y, X, .method = peek, label = "peek")
  est <- draw_estimates(design)
  expect_equal(est$y_got, "Y")
  expect_equal(est$x_got, "X")
})

test_that("a tidy-evaluating helper resolves its own column names", {
  pull_mean <- function(data, y) {
    tibble::tibble(term = "mean", estimate = mean(dplyr::pull(data, {{ y }})))
  }
  design <- declare_model(N = 20, Y = seq_len(N)) +
    declare_estimator(Y, .method = pull_mean, .summary = function(x) x,
                      label = "m")
  expect_equal(draw_estimates(design)$estimate, mean(1:20))
})

test_that("the ordinary estimator spellings are unaffected", {
  my_weights <- runif(60, 0.5, 1.5)
  model <- declare_model(N = 60, cl = rep(1:6, each = 10), U = rnorm(N),
                         Z = randomizr::complete_ra(N), Y = U + 0.4 * Z,
                         B = rbinom(N, 1, 0.5))

  plain <- declare_estimator(Y ~ Z, .method = estimatr::lm_robust, term = "Z",
                             label = "e")
  clustered <- declare_estimator(Y ~ Z, clusters = cl,
                                 .method = estimatr::lm_robust, term = "Z",
                                 label = "e")
  weighted <- declare_estimator(Y ~ Z, weights = my_weights,
                                .method = estimatr::lm_robust, term = "Z",
                                label = "e")
  logit <- declare_estimator(B ~ Z, family = binomial, .method = glm,
                             term = "Z", label = "e")

  for (step in list(plain, clustered, weighted, logit)) {
    est <- draw_estimates(model + step)
    expect_equal(nrow(est), 1L)
    expect_true(is.finite(est$estimate))
  }
})

test_that("a method wanting a plain vector needs the column named explicitly", {
  # The losing case of the rule, and the spelling that replaces it. This
  # matches DeclareDesign, where the same declaration errors the same way.
  mean_of <- function(data, v) tibble::tibble(term = "mean", estimate = mean(v))
  model <- declare_model(N = 10, Y = seq_len(N))

  expect_error(
    draw_estimates(model + declare_estimator(v = Y, .method = mean_of,
                                             .summary = function(x) x,
                                             label = "m")),
    "object 'Y' not found"
  )
  est <- draw_estimates(
    model + declare_estimator(handler = function(data) mean_of(data, data$Y),
                              label = "m")
  )
  expect_equal(est$estimate, mean(1:10))
})

test_that(".summary accepts the formula shorthand", {
  # declaration_9.3 in the book writes `.summary = ~tidy_stan(., exponentiate = TRUE)`.
  # DeclareDesign 1.1.1 accepts it; the rewrite called the formula as a function
  # and failed with `could not find function "summary_fn"`.
  design <- declare_model(N = 50, Y = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N)) +
    declare_estimator(Y ~ Z, .method = estimatr::lm_robust,
                      .summary = ~generics::tidy(.))
  expect_s3_class(draw_estimates(design), "data.frame")

  labelled <- label_estimator(
    function(data, ...) estimatr::lm_robust(Y ~ Z, data = data),
    label = "lin", .summary = ~generics::tidy(.)
  )
  expect_s3_class(labelled(draw_data(design)), "data.frame")
})

test_that("tidy_try falls back to the coefficient matrix when broom cannot tidy the fit", {
  coefs <- matrix(c(0.5, 0.1, 5, 0.01), nrow = 1,
                  dimnames = list("Z", c("Estimate", "Std. Error", "t value",
                                         "Pr(>|t|)")))
  assign("summary.ddtoyfit", function(object, ...) list(coefficients = coefs),
         envir = globalenv())
  assign("summary.ddbarefit", function(object, ...) list(), envir = globalenv())
  withr::defer(rm("summary.ddtoyfit", "summary.ddbarefit", envir = globalenv()))

  out <- DeclareDesign:::tidy_try(structure(list(), class = "ddtoyfit"))
  expect_equal(out$term, "Z")
  expect_equal(out$estimate, 0.5)
  expect_equal(out$std.error, 0.1)
  expect_equal(out$statistic, 5)
  expect_equal(out$p.value, 0.01)

  # No coefficient matrix either: an empty table of the right shape, so the
  # draw is a failed draw rather than a malformed one.
  bare <- DeclareDesign:::tidy_try(structure(list(), class = "ddbarefit"))
  expect_equal(nrow(bare), 0L)
  expect_equal(names(bare), c("term", "estimate", "std.error", "statistic",
                              "p.value", "conf.low", "conf.high"))
})

test_that("an inquiry may be a list or anything that is a string when coerced", {
  expect_equal(DeclareDesign:::normalize_inquiry(list("ATE", "ATT")),
               c("ATE", "ATT"))
  expect_equal(DeclareDesign:::normalize_inquiry(factor("ATE")), "ATE")
  expect_null(DeclareDesign:::normalize_inquiry(NULL))
})

test_that("a named term leaves a handler's table alone when it has no term column", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(handler = function(data) data.frame(estimate = 1),
                      term = "Z", label = "termless")
  expect_equal(draw_estimates(design)$estimate, 1)
})

test_that("a handler's table is filtered when a term is named and it has terms", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(handler = function(data) {
      data.frame(term = c("(Intercept)", "Z"), estimate = c(3, 7))
    }, term = "Z", label = "picky")
  estimates <- draw_estimates(design)
  expect_equal(estimates$term, "Z")
  expect_equal(estimates$estimate, 7)
})

test_that("more estimate rows than inquiries attaches the first inquiry", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_estimator(handler = function(data) {
      data.frame(term = c("a", "b", "c"), estimate = c(1, 2, 3))
    }, inquiry = c("A", "B"), label = "three")
  expect_equal(draw_estimates(design)$inquiry, rep("A", 3))
})

test_that("a failed draw keeps the inquiry the estimator named", {
  design <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    declare_inquiry(ATE = 0.5) +
    declare_estimator(handler = function(data) stop("no fit today"),
                      inquiry = "ATE", label = "broken")
  sims <- suppressWarnings(simulate_design(design, sims = 2))
  expect_true(all(sims$error))
  expect_equal(unique(sims$inquiry), "ATE")
})

test_that("legacy `model =` is read as `.method` with a deprecation warning", {
  # The warning is `.frequency = "once"`, so only the first call in the session
  # raises it. Everything about `model =` is asserted here, in one test, for
  # that reason: a second test elsewhere would pass or fail on file order.
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
  expect_equal(est$term, "Z")
  expect_equal(est$estimator, "ols")
  expect_false("model" %in% names(est))

  later <- suppressWarnings(
    declare_estimator(Y ~ Z, model = lm, term = "Z", label = "legacy"))
  expect_identical(attr(later, "method_name"), "lm")
})

test_that("declare_test reads a legacy `model =` as `.method` too", {
  # The deprecation warning is shared with declare_estimator() and fires once
  # per session, so it is asserted only in the test above.
  step <- suppressWarnings(
    declare_test(Y ~ Z, model = estimatr::lm_robust, term = "Z", label = "t"))
  expect_identical(attr(step, "method_name"), "lm_robust")
  design <- declare_model(N = 40, Z = rep(0:1, 20), Y = Z + rnorm(N)) + step
  est <- draw_estimates(design)
  expect_equal(est$term, "Z")
  expect_true(all(c("std.error", "df") %in% names(est)))
})

test_that("label_test is label_estimator under a second name", {
  expect_identical(label_test, label_estimator)
})

test_that("a method written inline is named `custom` rather than deparsed whole", {
  long <- function(formula, data) lm(formula, data = data)
  expect_equal(
    DeclareDesign:::method_expr_label(
      quote(function(formula, data) estimatr::lm_robust(formula, data = data))),
    "custom")
  expect_equal(DeclareDesign:::method_expr_label(quote(estimatr::lm_robust)),
               "lm_robust")
  expect_null(DeclareDesign:::method_expr_label(NULL))
})
