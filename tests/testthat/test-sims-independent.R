context("Test whether separate simulations are independent")

design <- declare_model(N = 100, U = rnorm(N)) + declare_inquiry(Q = mean(U))

test_that("diagnose_design when setting seed they are identical, when not setting seed they are independent", {
  
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  
  sims_a <- design |> diagnose_design(sims = 10, future.seed = 5) |> get_simulations()
  sims_b <- design |> diagnose_design(sims = 10, future.seed = 5) |> get_simulations()
  
  expect_equal(sims_a$estimand, sims_b$estimand) # exactly 1
  
  set.seed(20)
  
  sims_a <- design |> diagnose_design(sims = 10) |> get_simulations()
  sims_b <- design |> diagnose_design(sims = 10) |> get_simulations()
  
  expect_lt(abs(cor(sims_a$estimand, sims_b$estimand)), 0.4) # ~0 (should be, because didn't set seed in between)
  
  set.seed(5)
  sims_a <- design |> diagnose_design(sims = 10) |> get_simulations()
  set.seed(5)
  sims_b <- design |> diagnose_design(sims = 10) |> get_simulations()
  
  expect_equal(sims_a$estimand, sims_b$estimand) # exactly 1
  
})

test_that("diagnose_design focused on bootstrapping - when setting seed they are identical, when not setting seed they are independent", {
  
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  
  sims_a <- design |> diagnose_design(sims = 10, bootstrap_sims = 5, future.seed = 5) 
  sims_b <- design |> diagnose_design(sims = 10, bootstrap_sims = 5, future.seed = 5) 
  
  expect_equal(sims_a$bootstrap_replicates$mean_estimand, sims_b$bootstrap_replicates$mean_estimand) # exactly 1
  
  set.seed(5)
  
  sims_a <- design |> diagnose_design(sims = 10, bootstrap_sims = 5) 
  sims_b <- design |> diagnose_design(sims = 10, bootstrap_sims = 5) 
  
  expect_lt(abs(cor(sims_a$bootstrap_replicates$mean_estimand, sims_b$bootstrap_replicates$mean_estimand)), 0.4) # ~0 (should be, because didn't set seed in between)
  
  set.seed(5)
  sims_a <- design |> diagnose_design(sims = 10) 
  set.seed(5)
  sims_b <- design |> diagnose_design(sims = 10) 
  
  expect_equal(sims_a$bootstrap_replicates$mean_estimand, sims_b$bootstrap_replicates$mean_estimand) # exactly 1
  
})

test_that("simulate_design when setting seed they are identical, when not setting seed they are independent", {
  
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  
  sims_a <- design |> simulate_design(sims = 10, future.seed = 5)
  sims_b <- design |> simulate_design(sims = 10, future.seed = 5)
  
  expect_equal(sims_a$estimand, sims_b$estimand) # exactly 1
  
  set.seed(5)
  
  sims_a <- design |> simulate_design(sims = 10)
  sims_b <- design |> simulate_design(sims = 10)
  
  expect_lt(abs(cor(sims_a$estimand, sims_b$estimand)), 0.4) # ~0 (should be, because didn't set seed in between)
  
  set.seed(5)
  sims_a <- design |> simulate_design(sims = 10)
  set.seed(5)
  sims_b <- design |> simulate_design(sims = 10)
  
  expect_equal(sims_a$estimand, sims_b$estimand) # exactly 1
  
})

