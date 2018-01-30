context("Utilities")



test_that("get_unique_variables_by_level is identical to fabricatr version", {

  skip_on_cran()

  expect_equal(DeclareDesign:::get_unique_variables_by_level, fabricatr:::get_unique_variables_by_level)

})
