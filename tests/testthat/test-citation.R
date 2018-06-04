context("declare design citation")

test_that("test with generated citation", {


  design <- declare_design(
    sleep,
    meta = declare_citation(
      authors = "set of authors",
      title = "my design",
      description = "this is my text description of design"
    )
  )

  expect_output(cite_design(design), "my design")

  expect_output(print(design), 'Step \\d [(]citation[)]:') #
})

test_that("test with user-specified text citation", {

  text <- "Set of authors (2017). My custom design."

  design <- declare_design(
    sleep,
    declare_citation(citation=text)
  )

  expect_output(cite_design(design), text, fixed=TRUE)

})





