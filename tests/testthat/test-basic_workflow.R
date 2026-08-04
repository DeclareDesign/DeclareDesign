test_that("five-step design runs end to end", {
  design <- simple_design(N = 60, ate = 0.5)
  expect_s3_class(design, "design")
  expect_length(design, 5L)

  df <- draw_data(design)
  expect_true(nrow(df) == 60L)
  expect_true(all(c("Y_Z_0", "Y_Z_1", "Z", "Y") %in% names(df)))

  est_df <- draw_estimands(design)
  expect_equal(est_df$inquiry, "ATE")
  expect_equal(est_df$estimand, 0.5)

  est <- draw_estimates(design)
  expect_true("estimate" %in% names(est))
  expect_true("estimand" %in% names(est))
  expect_true("inquiry" %in% names(est))
})

test_that("simulate_design returns one row per sim", {
  design <- simple_design(N = 40)
  sims <- simulate_design(design, sims = 5)
  expect_equal(nrow(sims), 5L)
  expect_true("sim_ID" %in% names(sims))
  expect_true("estimate" %in% names(sims))
  expect_true("estimand" %in% names(sims))
})

test_that("diagnose_design produces standard diagnosands", {
  design <- simple_design(N = 40)
  d <- diagnose_design(design, sims = 10, bootstrap_sims = 0)
  expect_s3_class(d, "diagnosis")
  diag <- get_diagnosands(d)
  for (col in c("bias", "rmse", "power", "coverage")) {
    expect_true(col %in% names(diag))
  }
})

test_that("bootstrap SEs appear when requested", {
  design <- simple_design(N = 30)
  d <- diagnose_design(design, sims = 10, bootstrap_sims = 10)
  diag <- get_diagnosands(d)
  expect_true(any(grepl("^se\\(", names(diag))))
})

test_that("design + NULL returns the design unchanged", {
  d <- declare_model(N = 10, Y = rnorm(N))
  d2 <- d + NULL
  expect_s3_class(d2, "design")
  expect_length(d2, 1L)
})

test_that("run_design rejects non-design input", {
  expect_error(run_design(6), "must be a `design`")
  expect_error(run_design("not a design"), "must be a `design`")
})

test_that("run_design returns one data frame, not a list of three", {
  design <- simple_design(N = 40)
  one_run <- run_design(design)
  expect_s3_class(one_run, "data.frame")
  expect_equal(nrow(one_run), 1L)
  expect_true(all(c("inquiry", "estimand", "estimate") %in% names(one_run)))
  expect_false("sim_ID" %in% names(one_run))
})

test_that("an estimator with no inquiry = still finds the single inquiry", {
  design <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")

  one_run <- run_design(design)
  expect_equal(one_run$inquiry, "ATE")
  expect_equal(one_run$estimand, 0.5)

  sims <- simulate_design(design, sims = 5)
  expect_true("estimand" %in% names(sims))
  expect_equal(nrow(sims), 5L)

  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_false(is.na(get_diagnosands(d)$bias))
})

test_that("an unlabelled estimator is reported against each inquiry", {
  design <- declare_model(N = 40, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z")
  one_run <- run_design(design)
  expect_equal(one_run$inquiry, c("ATE", "ATT"))
  expect_equal(one_run$estimate, rep(one_run$estimate[1], 2))
})

test_that("estimates and inquiries match on the group columns they share", {
  # Regression test: joining on `inquiry` alone crossed the 3 groups against
  # the 3 groups, and the estimand a group was scored against was arbitrary.
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
  one_run <- expect_no_warning(run_design(design))
  expect_equal(nrow(one_run), 3L)
  expect_equal(one_run$estimate, one_run$estimand)
})

test_that("several unlabelled estimators against several inquiries warns", {
  design <- declare_model(N = 40, U = rnorm(N), X = rnorm(N),
                          Y_Z_0 = U, Y_Z_1 = U + 0.5) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "unadjusted") +
    declare_estimator(Y ~ Z + X, .method = lm, term = "Z", label = "adjusted")
  expect_warning(run_design(design), "multiplied the rows")
})

test_that("declare_step with handler = fabricate evaluates lazily", {
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatrZero::fabricate, X2 = X * 2)
  d <- pop + step
  df <- draw_data(d)
  expect_equal(df$X2, df$X * 2)
})

test_that("declare_step accepts the original fabricatr::fabricate as handler", {
  skip_if_not_installed("fabricatr")
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatr::fabricate, X2 = X * 2)
  df <- draw_data(pop + step)
  expect_equal(df$X2, df$X * 2)
})

test_that("fabricate still needs quosures, which is why its branch survives", {
  # `N` is bound by fabricate's own mask, not by anything the caller wrote.
  # Spliced quosures keep the user's environment and this works; bare
  # expressions carry the environment of the call we build and it does not.
  # The day this passes on the as-written path, handler_is_fabricate() can go.
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatrZero::fabricate, Y = X + rnorm(N, 0, 0))
  df <- draw_data(pop + step)
  expect_equal(df$Y, df$X)
})

test_that("declare_step passes tidyselect handlers the column names, not the values", {
  # Regression test for the trust-game design (RDSS declaration_17.6), reported
  # by the Live Designs app port. Evaluating the dots first handed pivot_wider
  # the *contents* of `role`, so tidyselect looked for columns named "A" and
  # "B" and errored. Arguments now arrive as written and the handler selects.
  long <- data.frame(pair = rep(1:3, each = 2), role = rep(c("A", "B"), 3),
                     ID = sprintf("%03d", 1:6), a = 1:6)
  step <- declare_step(id_cols = pair, names_from = role,
                       values_from = c(ID, a), handler = tidyr::pivot_wider)
  wide <- step(long)
  expect_equal(nrow(wide), 3L)
  expect_equal(names(wide), c("pair", "ID_A", "ID_B", "a_A", "a_B"))
  expect_equal(wide$a_A, c(1, 3, 5))
})

test_that("the app's quoted-name workaround keeps working", {
  # 17.6 currently ships quoted names to get around the above. Both spellings
  # have to work, or adopting the fix would force a coordinated release.
  long <- data.frame(pair = rep(1:3, each = 2), role = rep(c("A", "B"), 3),
                     ID = sprintf("%03d", 1:6), a = 1:6)
  step <- declare_step(id_cols = "pair", names_from = "role",
                       values_from = c("ID", "a"), handler = tidyr::pivot_wider)
  expect_equal(names(step(long)), c("pair", "ID_A", "ID_B", "a_A", "a_B"))
})

test_that("declare_step handlers that mask resolve data expressions themselves", {
  # dplyr verbs do their own masking, so `mean(a)` needs no special support.
  df <- data.frame(a = c(1, 2, 3, 4, 5, 6))
  expect_equal(declare_step(handler = dplyr::summarise, m = mean(a))(df)$m, 3.5)
  expect_equal(nrow(declare_step(handler = dplyr::filter, a > mean(a))(df)), 3L)
  expect_equal(sum(declare_step(handler = dplyr::mutate, hi = a > mean(a))(df)$hi), 3L)
})

test_that("declare_step still takes a plain value from the caller", {
  k <- 2
  step <- declare_step(handler = function(data, k) {
    data$X2 <- data$X * k
    data
  }, k = k)
  expect_equal(step(data.frame(X = 1:5))$X2, c(2, 4, 6, 8, 10))
})

test_that("draw_data and draw_estimands do not run the estimators", {
  # Regression test, from Macartan's crash course: an RDD design whose
  # estimator handler wanted bare column names failed, and it took
  # draw_data() down with it because every step was being run.
  ran <- FALSE
  design <- declare_model(N = 30, U = rnorm(N), Y = U) +
    declare_inquiry(mu = mean(Y)) +
    declare_estimator(handler = function(data) {
      ran <<- TRUE
      stop("this estimator is broken")
    }, label = "broken")

  expect_equal(nrow(draw_data(design)), 30L)
  expect_equal(draw_estimands(design)$inquiry, "mu")
  expect_false(ran)
  expect_error(run_design(design), "this estimator is broken")
})
