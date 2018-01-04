context("declare design citation")

my_population <- declare_population(N = 50)

my_assignment <- declare_assignment(m = 25)


test_that("test with generated citation", {


  design <- declare_design(
    my_population, my_assignment,
    declare_citation(
      authors = "set of authors",
      title = "my design",
      description = "this is my text description of design"
    )
  )

  expect_output(cite_design(design), "my design")


})

test_that("test with user-specified text citation", {

  text <- "Set of authors (2017). My custom design."

  design <- declare_design(
    my_population, my_assignment, declare_citation(citation=text))

  expect_output(cite_design(design), text, fixed=TRUE)

})





