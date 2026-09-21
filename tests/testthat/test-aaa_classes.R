test_that("a design can be built up from NULL", {
  # `NULL + step` is what makes Reduce() over a list of steps work, and it is
  # the same no-op as `step + NULL`, which the roxygen documents for adding a
  # step conditionally.
  steps <- list(declare_model(N = 5, Y = seq_len(N)),
                declare_inquiry(m = mean(Y)))
  design <- Reduce(`+`, steps, init = NULL)
  expect_s3_class(design, "design")
  expect_equal(run_design(design)$estimand, 3)
  expect_s3_class(NULL + declare_model(N = 3), "design")
  expect_s3_class(NULL + (declare_model(N = 3) + declare_inquiry(m = 1)), "design")
})

test_that("an estimator with no formula keeps the label it was given", {
  # The inferred label is the formula in the first dot. A handler-based
  # estimator has no formula to read, so there is nothing to infer and the
  # declared label stands.
  handler <- function(data) {
    data.frame(estimate = mean(data$Y), term = "mean")
  }
  design <- declare_model(N = 10, Y = seq_len(N)) +
    declare_estimator(handler = handler, label = "mine")
  expect_equal(names(design), c("model", "mine"))
  expect_equal(run_design(design)$estimate, 5.5)
  # Two of them collide on the default label and are suffixed apart.
  two <- suppressMessages(
    declare_model(N = 10, Y = seq_len(N)) +
      declare_estimator(handler = handler) +
      declare_estimator(handler = handler)
  )
  expect_equal(names(two), c("model", "estimator.a", "estimator.b"))
})
