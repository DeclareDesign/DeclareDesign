context("redesign")

test_that("N changing", {
  N <- 100
  design <- declare_model(N = N) + NULL
  expect_equal(N, 100)
  rm(N)  

  expect_true(DeclareDesign:::find_all_objects(design)$value_str == 100)
  expect_length(draw_data(design)$ID, 100)
  
  design2 <- redesign(design, N = 20)
  expect_length(draw_data(design2)$ID, 20)

  DeclareDesign:::find_all_objects(design2)
  draw_data(design2) |> nrow()
  
  others <- c(50, 100, 200, 99)
  d_alt <- redesign(design, N = others)

  for (i in seq_along(others)) {
    expect_length(draw_data(d_alt[[i]])$ID, others[i])
  }

})


test_that("message if meaningless change", {
  N <- 100
  design <- declare_model(N = N) + NULL
  expect_message(redesign(design, k = 2, N = 200), "You requested a change to k but k is not found in the design")
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
  
  DeclareDesign:::find_all_objects(d)
  d
  
  d2 <- redesign(d, b = .4)
  DeclareDesign:::find_all_objects(d2)
  d2
    
  x <- run_design(d2)
  expect_true(x$estimand == .4)
  expect_true(abs(x$estimand - x$estimate) < .4)
  attr(d2[[1]], "dots")
  

  d3 <- redesign(d, b = 10)
  x <- run_design(d3)
  expect_true(x$estimand == 10)
  expect_true(abs(x$estimand - x$estimate) < .4)
  
})




test_that("embedded parameter, multiple steps", {
  n <- 2000
  b <- .2
  
  d <- 
    declare_model(N = n,  Y = 1) + 
    declare_estimator(Y ~ 1) +
    declare_inquiry(Q = b)
  
  expect_true(run_design(d)$estimand == .2)
  
  rm(b)
  
  expect_true(run_design(d)$estimand == .2)
  
  DeclareDesign:::find_all_objects(d)
  d
  
  d2 <- redesign(d, b = .4)
  DeclareDesign:::find_all_objects(d2)
  d2

  # N is found after redesgin
  b <- 2
  simple_1 <- declare_model(N = 3,  Y = N + b) + NULL 
  simple_1[[1]]()
  
  simple_2 <- DeclareDesign:::modify_edit(simple_1, b = 3)
  simple_2[[1]]()


  
  

  # N is found after redesgin
  n <- 3
  b <- 2
  simple_1 <- declare_model(N = n,  Y = rnorm(N, b)) + NULL 
  simple_1[[1]]()
  
  simple_2 <- DeclareDesign:::modify_edit(simple_1, b = 3)
  simple_2[[1]]()

  simple_3 <- DeclareDesign:::redesign(simple_1, b = 3)
  simple_3[[1]]()
  


  #  OK
  d <- 
    declare_model(N = n, Y = rnorm(N, b)) + 
    NULL
  
  d2 <- redesign(d, b = .4)
  d[[1]]()
  d2[[1]]()


  # Not OK
  d <- 
    declare_model(N = n, Y = rnorm(N, b)) + 
    declare_inquiry(Q = b)
  
  d2 <- redesign(d, b = .4)
  d[[1]]()


  expect_error(d2[[1]](), NA)
  

  
})


test_that("data_step throwing error", {
  n <- 2000
  b <- .2

  # OK
  d <- 
    declare_model(N = n) + 
    declare_inquiry(Q = b)
  
  redesign(d, b = .4)
  
  # Not OK
  d <- 
    declare_model(N = n, Y = rnorm(N, b)) + 
    declare_inquiry(Q = b)
  
  d2 <- redesign(d, b = .4)

  attr(d[[1]], "dots")
  attr(d2[[1]], "dots")
  
  expect_error(d2[[1]](), NA)

    
})
