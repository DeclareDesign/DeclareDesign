# `declare_model()`, `declare_measurement()`, `declare_assignment()` and
# `declare_sampling()`: which data a step starts from, what a handler
# receives, and which step appends a row id.
#
# The fabricatr entry point all four call is test-fabricatr-contract.R.

test_that("a model step in first position starts from no data", {
  step <- declare_model(N = 25, X = rnorm(N))
  df <- step(NULL)
  expect_equal(nrow(df), 25L)
  expect_true("X" %in% names(df))
})

test_that("declare_measurement adds columns to existing data", {
  m <- declare_model(N = 20, U = rnorm(N))
  meas <- declare_measurement(Y = U + 1)
  d1 <- m(NULL)
  d2 <- meas(d1)
  expect_equal(nrow(d2), 20L)
  expect_true("Y" %in% names(d2))
})

test_that("declare_sampling filters on the S column by default", {
  step <- declare_sampling(S = rep(c(1, 0), length.out = 10))
  df <- data.frame(ID = seq_len(10))
  out <- step(df)
  expect_true(all(out$S == 1))
})

test_that("declare_sampling honors a filter expression", {
  step <- declare_sampling(X = seq_len(10), filter = X > 5)
  df <- data.frame(ID = seq_len(10))
  out <- step(df)
  expect_true(all(out$X > 5))
})

test_that("a step's label is inferred when none is given", {
  d <- declare_model(N = 5, Y = rnorm(N), label = "popgen")
  expect_equal(attr(d, "label"), "popgen")
})

test_that("each verb sets its step type and causal type", {
  m <- declare_model(N = 5, Y = rnorm(N))
  expect_equal(attr(m, "step_type"), "model")
  expect_equal(attr(m, "causal_type"), "dgp")

  i <- declare_inquiry(mu = mean(Y))
  expect_equal(attr(i, "step_type"), "inquiry")
  expect_equal(attr(i, "causal_type"), "inquiry")
})

test_that("declare_model accepts a custom handler", {
  myf <- function(N) data.frame(u = rnorm(N))
  step <- declare_model(handler = myf, N = 50)
  out <- step()
  expect_equal(nrow(out), 50L)
  expect_true("u" %in% names(out))
})

test_that("multilevel declare_model nests and draws independently per cluster", {
  # No test touched add_level or nest_level, which is how a recycling bug in
  # the nested path went unnoticed: every cluster received identical residuals.
  set.seed(4)
  design <- declare_model(
    villages = add_level(N = 25, u_v = rnorm(N)),
    citizens = add_level(N = 8, e = rnorm(N))
  )
  df <- draw_data(design)
  expect_equal(nrow(df), 200L)
  expect_equal(length(unique(df$villages)), 25L)
  expect_equal(length(unique(df$citizens)), 200L)
  # Cluster-level column is constant within village, unit-level column is not
  expect_equal(length(unique(tapply(df$u_v, df$villages, sd))), 1L)
  expect_true(all(is.na(tapply(df$u_v, df$villages, sd)) |
                    tapply(df$u_v, df$villages, sd) == 0))
  by_village <- split(df$e, df$villages)
  expect_equal(length(unique(by_village)), 25L)
})

test_that("declare_model may declare N alone and add variables downstream", {
  design <- declare_model(N = 80) +
    declare_measurement(Y = rbinom(N, 1, 0.5))
  df <- draw_data(design)
  expect_equal(nrow(df), 80L)
  expect_true("Y" %in% names(df))
})

test_that("a handler receives its arguments as written, not as values", {
  # `declare_step()` has always passed arguments as written; the fabricate-based
  # steps evaluated them first, which called dplyr verbs without their data and
  # left tidyselect handlers holding a vector where a column name belonged.
  base <- declare_model(N = 4, x = 1:4, wt = c(1, 2, 1, 2))

  plain <- draw_data(base + declare_model(y = x * 2, handler = dplyr::mutate))
  expect_equal(plain$y, c(2, 4, 6, 8))

  # Context functions are only defined inside a data-masking verb, so their
  # working is the evidence that `mutate()` did the evaluation.
  ctx <- draw_data(base + declare_model(k = dplyr::n(), handler = dplyr::mutate))
  expect_equal(unique(ctx$k), 4L)

  meas <- draw_data(base + declare_measurement(y = x * 2, handler = dplyr::mutate))
  expect_equal(meas$y, c(2, 4, 6, 8))
})

test_that("uncount as a handler drops the weights column it was given", {
  base <- declare_model(N = 4, x = 1:4, wt = c(1, 2, 1, 2))
  out <- draw_data(base + declare_model(handler = tidyr::uncount, weights = wt))
  expect_equal(nrow(out), 6L)
  expect_false("wt" %in% names(out))
})

test_that("a handler with no data argument is still called without data", {
  step <- declare_model(handler = function(N) data.frame(u = rnorm(N)), N = 12)
  expect_equal(nrow(step()), 12L)
})

test_that("resample_data as a handler takes a scalar N", {
  dataset <- data.frame(a = 1:5, b = 6:10)
  n_out <- 12
  out <- draw_data(declare_model(data = dataset, handler = fabricatr::resample_data,
                                 N = n_out))
  expect_equal(nrow(out), 12L)
  expect_equal(names(out), c("a", "b"))
})

test_that("a sampling step that keeps every row says so", {
  # `declare_sampling(S1 = ...)` filtered on nothing and returned every row.
  rlang::reset_warning_verbosity("dd_sampling_no_S")
  design <- declare_model(N = 20, U = rnorm(N)) +
    declare_sampling(S1 = sample(rep(0:1, 10)))
  expect_warning(df <- draw_data(design), "no column named `S`")
  expect_equal(nrow(df), 20L)
  quiet <- declare_model(N = 20, U = rnorm(N)) +
    declare_sampling(S = sample(rep(0:1, 10)))
  expect_silent(df <- draw_data(quiet))
  expect_equal(nrow(df), 10L)
})

test_that("declare_sampling() takes a custom handler, as the other data verbs do", {
  # A handler dot went straight to fabricate and died in rep() on a closure;
  # DesignLibrary's regression discontinuity designer sampled that way.
  design <- declare_model(N = 40, X = seq(-2, 2, length.out = N)) +
    declare_sampling(handler = function(data) data[abs(data$X) < 1, ])
  df <- draw_data(design)
  expect_true(all(abs(df$X) < 1))
  expect_lt(nrow(df), 40L)
  with_filter <- declare_model(N = 40, X = seq(-2, 2, length.out = N)) +
    declare_sampling(handler = function(data) data[abs(data$X) < 1, ], filter = X > 0)
  expect_true(all(draw_data(with_filter)$X > 0))
})

test_that("only a model step appends a row id", {
  # make_fabricate_step() carried an `id_label_na` flag, documented as "pass
  # ID_label = NA so fabricate does not append a row id", and never passed it
  # on. All four call sites set it deliberately and it reached nothing. It was
  # invisible because these steps normally run with data already in hand, and
  # fabricate() suppresses the flat id whenever data are present; a step that
  # supplies its own rows is where it shows. 1.x wrote `ID_label = NA` at each
  # of the four sites in declare_assignment.R, declare_measurement.R,
  # declare_potential_outcomes.R and declare_sampling.R.
  expect_equal(names(draw_data(declare_model(N = 4, Y = rnorm(N)))),
               c("ID", "Y"))
  expect_equal(names(draw_data(declare_measurement(N = 4, Y = rnorm(N)))), "Y")
  expect_equal(names(draw_data(declare_assignment(N = 4, Z = rbinom(N, 1, 0.5)))),
               "Z")
  expect_equal(names(draw_data(declare_sampling(N = 4, S = 1))), "S")
})

test_that("a step given data in hand is unchanged by the id rule", {
  # The ordinary shape: the model makes the rows and names them, and nothing
  # downstream adds a second id column.
  design <- declare_model(N = 5, X = rnorm(N)) +
    declare_measurement(Y = X * 2) +
    declare_assignment(Z = rbinom(N, 1, 0.5))
  expect_equal(names(draw_data(design)), c("ID", "X", "Y", "Z"))
})

test_that("`handler = fabricate` reaches fabricate's own formals", {
  # `fabricate()` reads `N` and `ID_label` off its formals, so a quosure
  # spliced into one arrived as a quosure object and the call died inside
  # fabricate with "given an object of class quosure". Every declaration that
  # spelled the handler out and named `N` was affected, and that is 1.x's
  # documented spelling: DeclareDesign 1.1.1 accepts all four of these.
  # Arguments reaching fabricate through `...` were fine throughout, which is
  # why the handler tests in test-basic-workflow.R passed over it.
  expect_equal(nrow(declare_model(handler = fabricate, N = 5)(NULL)), 5L)
  expect_equal(nrow(declare_sampling(handler = fabricate, N = 3)(NULL)), 3L)
  expect_equal(nrow(declare_assignment(handler = fabricate, N = 3)(NULL)), 3L)
  expect_equal(nrow(declare_measurement(handler = fabricate, N = 3)(NULL)), 3L)
  df <- declare_model(handler = fabricate, N = 4, X = seq_len(N))(NULL)
  expect_equal(df$X, 1:4)
})

test_that("spelling the handler out does not change the verb's id rule", {
  # The id belongs to the verb rather than to the handler: a model step names
  # its rows and the other three do not, on either spelling. 1.1.1 differs,
  # because there `fabricate` was declare_model()'s own default and the other
  # three verbs had handlers of their own, so naming it on a sampling step
  # asked for something else and got an `ID` column back.
  expect_equal(names(declare_model(handler = fabricate, N = 3)(NULL)), "ID")
  expect_equal(names(declare_model(N = 3)(NULL)), "ID")
  expect_equal(names(declare_sampling(handler = fabricate, N = 3)(NULL)),
               character(0))
  expect_equal(names(suppressWarnings(declare_sampling(N = 3)(NULL))),
               character(0))
  # A declaration that names `ID_label` gets it, on any verb.
  expect_equal(
    names(declare_model(handler = fabricate, N = 3, ID_label = "unit")(NULL)),
    "unit"
  )
  expect_equal(
    names(declare_sampling(handler = fabricate, N = 3, ID_label = "unit")(NULL)),
    "unit"
  )
})

test_that("a sampling step can supply its own data", {
  frame <- data.frame(ID = 1:6, S = rep(c(1, 0), 3))
  # The frame is used when the step is first in the pipeline, and ignored when
  # a previous step has already handed data along, which is the same rule
  # `declare_model(data = )` follows.
  expect_equal(declare_sampling(data = frame)(NULL)$ID, c(1L, 3L, 5L))
  expect_equal(
    nrow(declare_sampling(data = frame, S = c(1, 0, 1))(data.frame(ID = 1:3))),
    2L
  )
})
