

test_that("run design errors if you don't send a design object", {
  
  expect_error(run_design(6))
  
})
