context("wrap step")

my_population <- declare_population(N = 10)

test_that("dplyr verbs wrap", {
  library(dplyr)
  debugonce(tidy_step)
  a_wrapped_mutate <- tidy_step(mutate(q = 5))
  
  expect_equal(nrow(a_wrapped_mutate(my_population())), 10)
  
  des <- my_population + a_wrapped_mutate
  expect_equal(names(des), c("my_population", "a_wrapped_mutate"))
  
  des1 <- my_population + a_wrapped_mutate
  expect_equal(names(des1), c("my_population", "a_wrapped_mutate"))
  
  des2 <- declare_population(N = 10) + tidy_step(mutate(q = 5))
  expect_equal(names(des2), c("declare_population(N = 10)", "tidy_step(mutate(q = 5))"))
})


test_that("own functions wrap", {
  my_function <- function(data, my_mean) {
    data$new_variable <- rnorm(n = nrow(data), mean = my_mean)
    data
  }
  
  expect_error(des <- my_population + my_function, 
               regexp = "The right hand side does not appear to be a DeclareDesign object. Can you wrap the step with `tidy_step()`?")

  des <- my_population + tidy_step(my_function(my_mean = 2))
  expect_equal(names(des), c("my_population", "tidy_step(my_function(my_mean = 2))"))
  
  a_wrapped_step <- tidy_step(my_function(my_mean = 2))
  
  des <- my_population + a_wrapped_step
  expect_equal(names(des), c("my_population", "a_wrapped_step"))
})
