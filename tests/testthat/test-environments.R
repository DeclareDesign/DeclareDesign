
# case 1

test_that(" pars are saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  design <- declare_model(N = n, x = runif(N)) +
    declare_model(y = f(x, b)) 
  design
  rm(b, f)
  design
  expect_equal(nrow(draw_data(design)),5)
})

test_that(" n is saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  step <- declare_model(N = n, x = runif(N))
  expect_true(  nrow(step()) == 5)
  rm(n)
  expect_true(  nrow(step()) == 5)
})


