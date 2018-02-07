context("Utilities")



test_that("onLoad adds DD drat repo", {

  expect_equal(.onLoad()["declaredesign"], c(declaredesign="https://declaredesign.github.io"))

})
