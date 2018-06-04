
test_that("test error for extra comma and unnamed variables", {
  
  expect_error(declare_population(my_level = add_level(N = 50, my_var = rnorm(N)), ), 
               "Please name all variables inside your declaration. A possible cause of this error could be a hanging extra comma at the end of the list of variables. Please remove any extra commas.")
  
  expect_error(declare_population(some_var = rnorm(N), ), 
               "Please name all variables inside your declaration. A possible cause of this error could be a hanging extra comma at the end of the list of variables. Please remove any extra commas.")
  
  expect_error(declare_population(some_var = rnorm(N), rnorm(N)), 
               "Please name all variables inside your declaration. A possible cause of this error could be a hanging extra comma at the end of the list of variables. Please remove any extra commas.")
  
  a <- declare_population(my_level_1 = add_level(N = 5),
                          my_level_2 = add_level(N = 5),)
  expect_error(a())
})