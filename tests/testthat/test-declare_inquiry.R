test_that("declare_inquiry computes named scalar estimands", {
  step <- declare_inquiry(mu = mean(Y), med = median(Y))
  df <- data.frame(Y = c(1, 2, 3, 4, 5))
  out <- step(df)
  expect_equal(out$inquiry, c("mu", "med"))
  expect_equal(out$estimand, c(mean(df$Y), median(df$Y)))
})

test_that("declare_inquiry honors subset", {
  step <- declare_inquiry(mu = mean(Y), subset = group == "a")
  df <- data.frame(Y = 1:6, group = rep(c("a", "b"), 3))
  out <- step(df)
  expect_equal(out$estimand, mean(df$Y[df$group == "a"]))
})

test_that("declare_inquiry aliases work", {
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

test_that("ATT-style subset = Z == 1 evaluates inquiry on subset", {
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
