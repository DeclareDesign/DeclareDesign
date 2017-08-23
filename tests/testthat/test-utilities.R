context("Utilities")

test_that("Utilities",{
  expect_equal(format_num(2000), "2000.000")
  expect_equal(format_num(1234.5678), "1234.568")
  expect_equal(format_num(1234.5678, digits=1), "1234.6")
  expect_warning(format_num("Not a valid number"))
  suppressWarnings(expect_equal(format_num("Not a valid number"), "NA"))
})
