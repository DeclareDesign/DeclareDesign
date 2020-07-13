context("Names and Labels")


test_that("estimand labels work", {
  my_private_estimator <- function(data) {
    data.frame(estimate = median(data$Y)^2)
  }
  pop <- declare_population(N = 6, Y = rnorm(N))

  # Unmarked case
  mand <- declare_estimand(mean(Y))
  mator <- declare_estimator(estimand = mand, handler = label_estimator(my_private_estimator))
  design <- pop + mand + mator
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)

  expect_true(all(diagnosis$simulations_df$estimand_label == "estimand"))

  # declare_estimand(b = 2) --> Label is b
  mand_2 <- declare_estimand(some_stat = mean(Y))
  mator_2 <- declare_estimator(estimand = mand_2, handler = label_estimator(my_private_estimator))
  design <- pop + mand_2 + mator_2
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$estimand_label == "some_stat"))

  # declare_estimand(2, label = "b") -->  Label is b
  mand_3 <- declare_estimand(mean(Y), label = "a_label")
  mator_3 <- declare_estimator(estimand = mand_3, handler = label_estimator(my_private_estimator))
  design <- pop + mand_3 + mator_3
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$estimand_label == "a_label"))

  # declare_estimand(a = 2, label = "b") -->  Label is b
  mand_4 <- declare_estimand(some_stat = mean(Y), label = "a_label")
  mator_4 <- declare_estimator(estimand = mand_4, handler = label_estimator(my_private_estimator))
  design <- pop + mand_4 + mator_4
  names(design)
  diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
  diagnosis$simulations_df
  expect_true(all(diagnosis$simulations_df$estimand_label == "some_stat"))
})

test_that("multiple estimands", {
  pop <- declare_population(N = 6, Y = rnorm(N))
  mand <- declare_estimand(a1 = 1, a2 = 2, a3 = 3, label = "b")
  design <- pop + mand

  diagnosis <- diagnose_design(design, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(diagnosis$diagnosands_df$estimand_label %in% c("a1", "a2", "a3")))
})


test_that("label conflicts", {
  pop <- declare_population(N = 6, Y = rnorm(N))
  mand_1 <- declare_estimand(some_stat = mean(Y))
  mand_2 <- declare_estimand(some_stat = median(Y))
  expect_error(design <- pop + mand_1 + mand_2)

  expect_error(mand_1 <- declare_estimand(some_stat = mean(Y), some_stat = median(Y)))
})



test_that("step name conflicts in design", {
  pop <- declare_population(N = 6, Y = rnorm(N))
  assign_1 <- declare_assignment(m = 2)
  mand_1 <- declare_estimand(some_stat = mean(Y))
  expect_error(design <- pop + mand_1 + mand_1, "You have estimands with identical labels: some_stat\nPlease provide estimands with unique labels")
  expect_equal(names(pop + assign_1 + assign_1), c("pop", "assign_1", "assign_1_1"))
})
