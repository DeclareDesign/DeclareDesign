context("add design citation")

test_that("test with generated citation", {

  design <- 
    declare_population(data = sleep) + 
    declare_sampling(n = 10)
  
  design <- 
  set_citation(design,
               author = "Lovelace, Ada",
               title = "Notes",
               year = 1953,
               description = "This is a text description of a design")
  
  cite <- cite_design(design)
  
  expect_equal(cite, 
               structure(list(structure(list(title = "Notes", author = structure(list(
                 list(given = NULL, family = "Lovelace", role = NULL, email = NULL, 
                      comment = NULL), list(given = NULL, family = "Ada", role = NULL, 
                                            email = NULL, comment = NULL)), class = "person"), note = "This is a text description of a design", 
                 year = "1953"), bibtype = "Unpublished")), class = "bibentry"))
  
})

test_that("test with user-specified text citation", {

  text <- "Set of authors (2017). My custom design."

  design <- 
    declare_population(data = sleep) + NULL
  
  design <- 
    set_citation(design, citation = text)
  
  expect_equal(cite_design(design), text)


})





