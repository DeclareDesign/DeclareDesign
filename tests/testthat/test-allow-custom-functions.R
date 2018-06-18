library(dplyr)

my_pop <- declare_population(N = 100)

# case 1

test_that("a bare function can be used in a design", {
  
  my_simple_func <- function(data){
    data$my_var <- 5
    data
  }
  
  des <- my_pop + my_simple_func
  dat <- draw_data(des)
  
  expect_equals(names(dat), c("ID", "my_var"))
  
})


# case 2

test_that("a function call can be used in a design", {
  
  `%dd%` <- DeclareDesign:::`+.dd`
  
  my_func <- function(data, my_n = 50){
    data %>% sample_n(n = my_n)
  }
  
  # errors before sending to our operator function
  my_pop + my_func(my_n = 50)
  
  # sends into our operator function
  my_pop %dd% my_func(my_n = 50)
  
  dat <- draw_data(des)
  
  expect_equals(nrow(dat), 50)
  
})
