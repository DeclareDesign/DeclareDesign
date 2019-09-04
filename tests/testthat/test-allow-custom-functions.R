library(dplyr)

my_pop <- declare_population(N = 100)

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

  # include without parens
  des <- my_pop + . %>% mutate(my_var = 5)
  dat <- draw_data(des)

  expect_equal(names(dat), c("ID", "my_var"))

  # include with parens
  des <- my_pop + (. %>% mutate(my_var = 5))
  dat <- draw_data(des)

  expect_equal(names(dat), c("ID", "my_var"))
})

# Use dyplr functions as handlers ?

test_that("dplyr functions can be handlers", {

  design2 <- declare_population(N = 5, X = rnorm(N)) + declare_step(Y = 4, handler = mutate)

  df <- draw_data(design2)

  expect_equal(df$Y, rep(4,5))
})

