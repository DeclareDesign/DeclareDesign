

context("naming steps")

test_that("design steps are named appropriately when blank", {
  pop <- declare_model(N = 25)
  des <- pop + declare_model(N = 50)
  expect_equal(names(des), c("pop", "model"))
  
  smp <- declare_sampling(S = complete_rs(N, n = 25))
  des <- declare_model(N = 50) + smp
  expect_equal(names(des), c("model", "smp"))
  
  des <-
    declare_model(N = 50) + 
    declare_model(Y_Z_0 = rnorm(N), Y_Z_1 = Y_Z_0 + 1) + 
    declare_inquiry(mean(Y_Z_1 - Y_Z_0)) + 
    declare_sampling(S = complete_rs(N, n = 25)) + 
    declare_assignment(Z = complete_ra(N, m = 10)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z)
  expect_equal(
    names(des),
    c(
      "model",
      "model_1",
      "inquiry",
      "sampling",
      "assignment",
      "measurement",
      "estimator"
    )
  )
})
