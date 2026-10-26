# `declare_step()`: a custom step, whose handler receives its arguments as
# written and resolves them itself.
#
# `fabricate()` is the one exception, and takes quosures (see
# `handler_is_fabricate()`). Redesigning a custom step is test-redesign.R, and
# the book's use of one is test-book-idioms.R.

test_that("a fabricate handler evaluates its arguments against the data it is handed", {
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricate, X2 = X * 2)
  df <- draw_data(pop + step)
  expect_equal(df$X2, df$X * 2)

  step <- declare_step(handler = fabricatr::fabricate, X2 = X * 2)
  df <- draw_data(pop + step)
  expect_equal(df$X2, df$X * 2)
})

test_that("declare_step passes fabricate its own formals, not quosures", {
  # `N` is a formal of `fabricate()` rather than one of its dots, so a spliced
  # quosure reached it unevaluated and the step errored. The other fabricate
  # tests here pass arguments through `...`, where eval_tidy unwraps them,
  # which is why the branch looked exercised.
  expect_equal(nrow(declare_step(handler = fabricatr::fabricate, N = 4)(NULL)), 4L)
})

test_that("fabricate still needs quosures, which is why its branch survives", {
  # `N` is bound by fabricate's own mask, not by anything the caller wrote.
  # Spliced quosures keep the user's environment and this works; bare
  # expressions carry the environment of the call we build and it does not.
  # The day this passes on the as-written path, handler_is_fabricate() can go.
  pop <- declare_model(N = 10, X = seq_len(N))
  step <- declare_step(handler = fabricatr::fabricate, Y = X + rnorm(N, 0, 0))
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

test_that("quoted column names reach a tidyselect handler as well", {
  # The Live Designs app's 17.6 ships quoted names to get around the above.
  # Both spellings have to work, or adopting the fix would force a coordinated
  # release.
  long <- data.frame(pair = rep(1:3, each = 2), role = rep(c("A", "B"), 3),
                     ID = sprintf("%03d", 1:6), a = 1:6)
  step <- declare_step(id_cols = "pair", names_from = "role",
                       values_from = c("ID", "a"), handler = tidyr::pivot_wider)
  expect_equal(names(step(long)), c("pair", "ID_A", "ID_B", "a_A", "a_B"))
})

test_that("a handler that masks resolves data expressions itself", {
  # dplyr verbs do their own masking, so `mean(a)` needs no special support.
  df <- data.frame(a = c(1, 2, 3, 4, 5, 6))
  expect_equal(declare_step(handler = dplyr::summarise, m = mean(a))(df)$m, 3.5)
  expect_equal(nrow(declare_step(handler = dplyr::filter, a > mean(a))(df)), 3L)
  expect_equal(sum(declare_step(handler = dplyr::mutate, hi = a > mean(a))(df)$hi), 3L)
})

test_that("a plain value from the caller reaches the handler as a value", {
  k <- 2
  step <- declare_step(handler = function(data, k) {
    data$X2 <- data$X * k
    data
  }, k = k)
  expect_equal(step(data.frame(X = 1:5))$X2, c(2, 4, 6, 8, 10))
})
