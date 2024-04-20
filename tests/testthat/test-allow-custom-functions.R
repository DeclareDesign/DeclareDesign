my_pop <- declare_model(N = 100)

# case 1

test_that("a bare function can be used in a design", {
  my_simple_func <- function(data) {
    data$my_var <- 5
    data
  }

  des <- my_pop + my_simple_func
  dat <- draw_data(des)

  expect_equal(names(dat), c("ID", "my_var"))
})


# case 2

test_that("a dplyr pipeline can be used in a design", {
  
  skip_if_not_installed("dplyr")
  
  library(dplyr)

  # include without parens
  des <- my_pop + function(data) data |> mutate(my_var = 5)
  dat <- draw_data(des)

  expect_equal(names(dat), c("ID", "my_var"))

  # include with parens
  des <- my_pop + (function(data) data |> mutate(my_var = 5))
  dat <- draw_data(des)

  expect_equal(names(dat), c("ID", "my_var"))
})

# Use dyplr functions as handlers ?

test_that("dplyr::mutate can be handlers", {
  
  skip_if_not_installed("dplyr")

  design2 <- declare_model(N = 5, X = rnorm(N)) + declare_step(Y = 4, handler = mutate)

  df <- draw_data(design2)

  expect_equal(df$Y, rep(4,5))
})

test_that("dplyr filter can be handlers", {
  
  skip_if_not_installed("dplyr")
  
  design2 <- declare_model(N = 5, X = rnorm(N)) + declare_step(ID > 3, handler = filter)
  
  df <- draw_data(design2)
  
  expect_equal(df$ID, as.character(4:5))
 expect_true(DeclareDesign:::is_implicit_data_arg(environment(design2[[2]])$dots))
})

test_that("dplyr filter can be handlers with explicit .data", {
  
  skip_if_not_installed("dplyr")
  
  design2 <- declare_model(N = 5, X = rnorm(N)) + declare_step(.data=data, ID > 3, handler = filter)
  
  df <- draw_data(design2)
  
  expect_equal(df$ID, as.character(4:5))
 expect_false(DeclareDesign:::is_implicit_data_arg(environment(design2[[2]])$dots))
})

