context("Utilities")

test_that("format_num",{
  expect_equal(format_num(2000), "2000.000")
  expect_equal(format_num(1234.5678), "1234.568")
  expect_equal(format_num(1234.5678, digits=1), "1234.6")
  expect_warning(format_num("Not a valid number"))
  suppressWarnings(expect_equal(format_num("Not a valid number"), "NA"))
})


test_that("get_unique_variables_by_level is identical to fabricatr version", {

  skip_on_cran()

  expect_equal(DeclareDesign:::get_unique_variables_by_level, fabricatr:::get_unique_variables_by_level)

})
