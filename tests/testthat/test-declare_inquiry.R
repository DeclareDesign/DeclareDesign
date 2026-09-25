# `declare_inquiry()` and its aliases: named estimands, subsets, labels, and
# custom handlers.

test_that("declare_inquiry computes named scalar estimands", {
  step <- declare_inquiry(mu = mean(Y), med = median(Y))
  df <- data.frame(Y = c(1, 2, 3, 4, 5))
  out <- step(df)
  expect_equal(out$inquiry, c("mu", "med"))
  expect_equal(out$estimand, c(mean(df$Y), median(df$Y)))
})

test_that("declare_inquiry evaluates on its subset", {
  step <- declare_inquiry(mu = mean(Y), subset = group == "a")
  df <- data.frame(Y = 1:6, group = rep(c("a", "b"), 3))
  out <- step(df)
  expect_equal(out$estimand, mean(df$Y[df$group == "a"]))
})

test_that("the declare_inquiry aliases all build an inquiry step", {
  s1 <- declare_inquiries(mu = mean(Y))
  s2 <- declare_estimand(mu = mean(Y))
  s3 <- declare_estimands(mu = mean(Y))
  for (s in list(s1, s2, s3)) {
    expect_equal(attr(s, "step_type"), "inquiry")
  }
})

test_that("a single splat-name promotes to the step label", {
  pate <- declare_inquiry(pate = mean(Y))
  expect_equal(attr(pate, "label"), "pate")
})

test_that("multiple splats keep the default label", {
  step <- declare_inquiry(mu = mean(Y), med = median(Y))
  expect_equal(attr(step, "label"), "inquiry")
})

test_that("an ATT is an inquiry on the subset Z == 1", {
  step <- declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0), subset = Z == 1)
  df <- data.frame(Y_Z_0 = 1:10, Y_Z_1 = 3:12,
                   Z = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
  out <- step(df)
  expect_equal(out$estimand, 2)
  expect_equal(out$inquiry, "ATT")
})

test_that("a single unnamed inquiry uses the step label as inquiry name", {
  default_step <- declare_inquiry(mean(Y))
  manual_step  <- declare_inquiry(mean(Y), label = "ATE2")
  df <- data.frame(Y = 1:5)
  expect_equal(default_step(df)$inquiry, "inquiry")
  expect_equal(manual_step(df)$inquiry, "ATE2")
})

test_that("custom inquiry handler receives `label` when it has that formal", {
  fn <- function(data, label) {
    data.frame(inquiry = label,
               estimand = median(data$Y_Z_1 - data$Y_Z_0))
  }
  step <- declare_inquiry(handler = fn, label = "medianTE")
  df <- data.frame(Y_Z_0 = 1:10, Y_Z_1 = 3:12)
  out <- step(df)
  expect_equal(out$inquiry, "medianTE")
  expect_equal(out$estimand, 2)
})

test_that("unnamed inquiries take the step's label, with a suffix when there is more than one", {
  one <- declare_model(N = 10, Y = 1:10) + declare_inquiry(mean(Y), label = "mu")
  expect_equal(draw_estimands(one)$inquiry, "mu")

  two <- declare_model(N = 10, Y = 1:10) +
    declare_inquiry(mean(Y), median(Y), label = "mu")
  expect_equal(draw_estimands(two)$inquiry, c("mu_1", "mu_2"))
  expect_equal(draw_estimands(two)$estimand, c(5.5, 5.5))
})

test_that("an inquiry handler's table gets the columns the design reads", {
  labelled <- declare_model(N = 10, Y = 1:10) +
    declare_inquiry(handler = function(data) data.frame(estimand = mean(data$Y)),
                    label = "mu")
  expect_equal(draw_estimands(labelled)$inquiry, "mu")

  # No `estimand` either: the first numeric column becomes it.
  numeric_only <- declare_model(N = 10, Y = 1:10) +
    declare_inquiry(handler = function(data) data.frame(value = c(2, 4)),
                    label = "mu")
  estimands <- draw_estimands(numeric_only)
  expect_equal(estimands$inquiry, c("mu", "mu"))
  expect_equal(estimands$estimand, c(2, 4))

  # Nothing numeric to promote: the step returns what the handler gave.
  no_number <- declare_model(N = 10, Y = 1:10) +
    declare_inquiry(handler = function(data) data.frame(note = "none"),
                    label = "mu")
  expect_false("estimand" %in% names(draw_estimands(no_number)))
})
