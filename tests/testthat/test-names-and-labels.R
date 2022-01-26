context("Names and Labels")


test_that("inquiry labels work", {
  my_private_estimator <- function(data) {
    data.frame(estimate = median(data$Y)^2)
  }
  pop <- declare_model(N = 6, Y = rnorm(N))

  # Unmarked case
  inquiry <- declare_inquiry(mean(Y))
  estimator <- declare_estimator(inquiry = inquiry, handler = label_estimator(my_private_estimator))
  design <- pop + inquiry + estimator
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)

  expect_true(all(diagnosis$simulations_df$inquiry == "inquiry"))

  # declare_inquiry(b = 2) --> Label is b
  inquiry_2 <- declare_inquiry(some_stat = mean(Y))
  mator_2 <- declare_estimator(inquiry = inquiry_2, handler = label_estimator(my_private_estimator))
  design <- pop + inquiry_2 + mator_2
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$inquiry == "some_stat"))

  # declare_inquiry(2, label = "b") -->  Label is b
  inquiry_3 <- declare_inquiry(mean(Y), label = "a_label")
  mator_3 <- declare_estimator(inquiry = inquiry_3, handler = label_estimator(my_private_estimator))
  design <- pop + inquiry_3 + mator_3
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$inquiry == "a_label"))

  # declare_inquiry(a = 2, label = "b") -->  Label is b
  inquiry_4 <- declare_inquiry(some_stat = mean(Y), label = "a_label")
  mator_4 <- declare_estimator(inquiry = inquiry_4, handler = label_estimator(my_private_estimator))
  design <- pop + inquiry_4 + mator_4
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$inquiry == "some_stat"))
})

test_that("multiple inquiries", {
  pop <- declare_model(N = 6, Y = rnorm(N))
  inquiry <- declare_inquiry(a1 = 1, a2 = 2, a3 = 3, label = "b")
  design <- pop + inquiry

  diagnosis <- diagnose_design(design, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(diagnosis$diagnosands_df$inquiry %in% c("a1", "a2", "a3")))
})


test_that("label conflicts", {
  pop <- declare_model(N = 6, Y = rnorm(N))
  inquiry_1 <- declare_inquiry(some_stat = mean(Y))
  inquiry_2 <- declare_inquiry(some_stat = median(Y))
  expect_error(design <- pop + inquiry_1 + inquiry_2)

  expect_error(inquiry_1 <- declare_inquiry(some_stat = mean(Y), some_stat = median(Y)))
})



test_that("step name conflicts in design", {
  pop <- declare_model(N = 6, Y = rnorm(N))
  assign_1 <- declare_assignment(Z = complete_ra(N, m = 2))
  inquiry_1 <- declare_inquiry(some_stat = mean(Y))
  expect_error(design <- pop + inquiry_1 + inquiry_1, "You have inquiries with identical labels: some_stat\nPlease provide inquiries with unique labels")
  expect_equal(names(pop + assign_1 + assign_1), c("pop", "assign_1", "assign_1_1"))
})
