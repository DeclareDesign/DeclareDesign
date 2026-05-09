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
