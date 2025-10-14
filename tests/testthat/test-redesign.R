context("redesign")

test_that("modify many steps", {
  
  a = 1
  b = 2
  c = 3
  f = 4

  design <- 
    declare_model(N = a, x1 = b) + 
    declare_measurement(x2 = b, x3 = c) +
    declare_inquiry(
      q1 = mean(x1),
      q2 = mean(x2),
      q3 = mean(x3),
      q4 = f
    )

  design_2 <- redesign(design, a = 2, b = 1, c = 4, f = 5) 
  
  expect_true(all((design |> draw_estimands())$estimand == c(2,2,3,4)))

  expect_true(all((design_2 |> draw_estimands())$estimand == c(1,1,4,5)))
  
})


test_that("changing handlers", {
  
  x <- 2
  
  my_handler <- function(N) data.frame(X = 1:2)
  
  design <-
    declare_model(
      handler = my_handler) +
    declare_model(X = x)
  
  find_all_objects(design)
  
  my_handler_2 <- function(N) data.frame(X = 1:3)
  
  design_2 <- modify_edit(design, my_handler = my_handler_2)
  
  expect_true(
    draw_data(redesign(design, my_handler = my_handler_2)) |> nrow() == 3
  )
  
})


test_that("modify functions", {
  
  b = -1
  a = .1
  f = function(x, a) a*x + b
  
  design <- 
    declare_model(N = 1, x = 0) + 
    declare_measurement(Y = f(x, a))
  
  design_2 <- redesign(design, f = function(x, a) 7)
  
  expect_true((design_2 |> draw_data())$Y == 7)
  
})


test_that("modify data", {
  
  df <- data.frame(X = 1)
  
  design <- 
    declare_model(data = df) + 
    declare_measurement(Y = 2 * X)

  design_2 <- modify_edit(design, df = data.frame(X = 1:2, Z = 3))
  expect_true(design_2 |> draw_data() |> nrow() == 2)

  design_2 <- redesign(design, df = list(data.frame(X = 1:2, Z = 3)))
  expect_true(design_2 |> draw_data() |> nrow() == 2)
  
})


test_that("N changing", {
  N <- 100
  design <- declare_model(N = N) + NULL
  expect_equal(N, 100)
  rm(N)  

  expect_true(find_all_objects(design)$value_str == 100)
  expect_length(draw_data(design)$ID, 100)
  
  design2 <- redesign(design, N = 20)
  expect_length(draw_data(design2)$ID, 20)

  find_all_objects(design2)
  draw_data(design2) |> nrow()
  
  others <- c(50, 100, 200, 99)
  d_alt <- redesign(design, N = others)

  for (i in seq_along(others)) {
    expect_length(draw_data(d_alt[[i]])$ID, others[i])
  }

})


test_that("modify inside a handler", {
  
  m <- 2
  b <- 100
  N <- pi  # distraction
  f <- function(...) fabricate(...)
  
  hdl <- function(...) f(..., extra = rnorm(N, b, 0))
  
  design <- 
    declare_model(N = m, U = rnorm(N)) + 
    declare_measurement(handler = hdl)
  
  rm(N, b, f, hdl)
  
  obs <- find_all_objects(design)
  # Rm N
  expect_true(all(obs$name == c("m", "b", "f", "hdl")))
  expect_true(mean(draw_data(design)$extra) == 100)
  
  
  design <- design|> redesign(b = -100)

  design_2 <- modify_edit(design, b = -100)
  obs <- find_all_objects(design_2)
  
  expect_true(mean(draw_data(design_2)$extra) == -100)
  
})


test_that("warning if meaningless change", {
  N <- 100
  design <- declare_model(N = N) + NULL
  expect_warning(redesign(design, k = 2, N = 200), "You requested a change to k but k is not found in the design")
})


test_that("embedded parameter, multiple steps", {
  n <- 5000
  b <- .2
  d <- 
    declare_model(N = n) + 
    declare_model(Y = rnorm(N, b)) + 
    declare_estimator(Y ~ 1) +
    declare_inquiry(Q = b)
  
  expect_true(run_design(d)$estimand == .2)
  rm(b)
  expect_true(run_design(d)$estimand == .2)
  
  d2 <- redesign(d, b = .4)

  x <- run_design(d2)
  expect_true(x$estimand == .4)
  expect_true(abs(x$estimand - x$estimate) < .4)


  d3 <- redesign(d, b = 10)
  x <- run_design(d3)
  expect_true(x$estimand == 10)
  expect_true(abs(x$estimand - x$estimate) < .4)
  
})



test_that("data_step OK", {
  
  n <- 1
  b <- .2

  d <- 
    declare_model(N = n, Y = b) + 
    declare_inquiry(Q = b) 
    
  
  d <- redesign(d, b = .3)

  expect_true(d[[1]]()$Y == .3)

})


test_that("future: changes inside functions", {

  b = -1
  a = .1
  f = function(x, a) a*x + b

  design <- 
    declare_model(N = 1, x = 0) + 
    declare_measurement(Y = f(x, a))
  
  find_all_objects(design)
  
  rm(b, a, f)
  
  expect_true((design |> draw_data())$Y == -1)
})





test_that("future: modify d; avoid partial matching problems", {
  
  d = 1

  design <- 
    declare_model(N = 1, D = d) + NULL

  expect_error(modify_edit(design = design, d=2), NA) 
  expect_error(redesign(design = design, d=2)) 
  

})
