context("declare design")

test_that("test with generated citation", {

  my_population <- declare_population(N = 50)

  my_assignment <- declare_assignment(m = 25)

  design <- declare_design(
    my_population, my_assignment, authors = "set of authors", title = "my design",
    description = "this is my text description of design")

  capture.output(cite_design(design))

  summary(design)

})

test_that("test with user-specified text citation", {

  my_population <- declare_population(N = 50)

  my_assignment <- declare_assignment(m = 25)

  design <- declare_design(
    my_population, my_assignment, citation = "Set of authors (2017). My custom design.")

  capture.output(cite_design(design))

})





