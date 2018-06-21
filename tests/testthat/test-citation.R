context("add design citation")

test_that("test with generated citation", {

  design <- 
    declare_population(data = sleep) + 
    declare_sampling(n = 10)
  
  design <- 
  add_citation(design,
               author = "Lovelace, Ada",
               title = "Notes",
               year = 1953,
               description = "This is a text description of a design")
  
  cite_design(design)
})

test_that("test with user-specified text citation", {

  text <- "Set of authors (2017). My custom design."

  design <- 
    declare_population(data = sleep) + NULL
  
  design <- 
    add_citation(design, citation = text)
  
  cite_design(design)


})





