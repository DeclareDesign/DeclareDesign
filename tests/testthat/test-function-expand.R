
context("functions in templates")

my_template <- function(N = 100, my_estimand_func = mean){ 
  my_pop <- declare_population(N = N, Y = rnorm(N))
  my_estimand <- declare_estimand(mand = my_estimand_func(Y))
  my_design <- declare_design(my_pop, my_estimand)
  my_design
}

test_that("raw templates can have functions as arguments", {
  
  expect_s3_class(my_template(my_estimand_func = median), "design")
  
})

test_that("sending function to expand_design", {
  prob <- expand_design(my_template, my_estimand_func = median)
  
  
})