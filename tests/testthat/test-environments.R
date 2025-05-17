
# case 1

test_that(" pars are saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  design <- 
    declare_model(N = n, x = runif(N)) +
    declare_model(y = f(x, b)) 
  design
  rm(b, f)
  design
  expect_equal(nrow(draw_data(design)),5)
})

test_that("n is saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  step <- declare_model(N = n, x = runif(N))
  expect_true(  nrow(step()) == 5)
  rm(n)
  expect_true(  nrow(step()) == 5)
})


test_that("deeper arguments  saved", {
  n <- 5
  b <- 0
  f <- function(x) b*x
  step <- declare_model(N = n, x = runif(N), y = f(x))
  expect_true(  mean(step()$y) == 0)
  rm(b)
  expect_true(  mean(step()$y) == 0)
})


test_that("data saved", {
  ddf <- data.frame(X = runif(5))
  step <- declare_model(data = ddf)
  step()
  rm(ddf)
  expect_true(nrow(step()) == 5)
})


# edge case: 
test_that("data called 'df' (or other function name)", {
  df <- data.frame(X = runif(5))
  step <- declare_model(data = df)
  step()
  rm(df)
  expect_true(nrow(step()) == 5)
})

test_that("estimator steps", {
  n <- 100
  b <- B <- .2

  d <- 
    declare_model(N = n, Y = runif(N)) + 
    declare_estimator(Y ~ 1, subset = Y < b)
  
  expect_true(draw_estimates(d)$estimate < B)

  rm(b)

  expect_true(draw_estimates(d)$estimate < B)
})

test_that("multiple steps", {
  n <- 1000
  b <- .2
  
  d <- 
    declare_model(N = n, Y = rnorm(N, b)) + 
    declare_estimator(Y ~ 1) +
    declare_inquiry(Q = b)
  
  expect_true(run_design(d)$estimand == .2)
  
  rm(b)
  
  expect_true(run_design(d)$estimand == .2)

    d <- redesign(d, b = .4)
    x <- run_design(d)
    expect_true(x$estimand == .4)
    expect_true(abs(x$estimand - x$estimate) < .1)

    d <- redesign(d, b = 10)
    x <- run_design(d)
    expect_true(x$estimand == 10)
    expect_true(abs(x$estimand - x$estimate) < .1)
    
})


test_that("custom estimator", {

  my_estimator <- function(data) {
    data.frame(estimate = mean(data$Y))
  }


  design <-
    declare_model(
      N = 500,
      Y = rnorm(N, sd = 0.25)
    ) +
    declare_inquiry(Y_bar = mean(Y)) +
    declare_estimator(handler = label_estimator(my_estimator),
                      label = "mean",
                      inquiry = "Y_bar")
  
  expect_true(nrow(run_design(design)) ==1)
  
  rm(my_estimator)

  expect_true(nrow(run_design(design)) ==1)


})



test_that("potential outcomes environment", {
  
  a <- .2
  b <- 2
  m <- 
    declare_model(
      N = 5,
      U = rnorm(N, sd = a),
      potential_outcomes(Y ~ b)
      )
  
  expect_true(nrow(m()) ==5)
  
  expect_true(environment(environment(m)$dots$U)$a ==a)
  expect_true(environment(environment(m)$dots[[4]])$b ==b)
  rm(a,b)
  expect_true(nrow(m()) ==5)
  
  # Some mad self referencing?
  #   environment(environment(environment(m)$quoData)$quoData)$quoData
  # f<- function(x)   environment(x)$quoData
  # f(m) 
  # f(f(m))
  # environment(m)$dots$U
  # environment(environment(m)$dots$U)$a
  
})



test_that("check not overriding pipe", {
  
  U <- 1:5
  m <- 
    declare_model(
      N = 5,
      U = rnorm(N),
      Y = U
    )

  # Global U is scooped up but not actually required  or used
  expect_true(all(environment(environment(m)$dots$Y)$U == U))
  
  expect_true(m()$Y[1] !=1)
  
})


# Issue here
test_that("check not overriding pipe", {
  
n1 <- 3
n2 <- 4

m <-   
  declare_model(
    classrooms = add_level(n1),
    individuals = add_level(n2)
  ) 

rm(n1, n2)

expect_true(m() |> nrow() ==12)


})
