context("declare design")

test_that(
  "test the full declare design setup",
  {
    N <- 500
    
    my_population <- declare_population(N = N, noise = rnorm(N))
    
    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    
    my_sampling <- declare_sampling(n = 250)
    
    my_assignment <- declare_assignment(m = 25)
    
    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
    
    my_reveal <- declare_reveal()
    
    design <- my_population +
      my_potential_outcomes +
      my_sampling +
      my_estimand +
      tidy_step(dplyr::mutate(q = 5)) +
      my_assignment +
      my_reveal +
      my_estimator
    
  df <- (draw_data(design))
  expect_equal(dim(df), c(250,9))

  output <- run_design(design)
  expect_equal(dim(output$estimates_df), c(1,8))
  expect_equal(dim(output$estimands), c(1,2))

})


test_that("No estimators / estimands", {

  design <- 
    declare_population(N = 500, noise = 1:N) + 
    declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 1) + 
    declare_sampling(n = 250) + 
    declare_assignment(m = 25) + 
    declare_reveal()

  head(draw_data(design))
  expect_identical(
    run_design(design),
    structure(list(estimates_df = structure(list(), class = "data.frame", row.names = integer(0)),
    estimands_df = structure(list(), class = "data.frame", row.names = integer(0))), .Names = c("estimates_df",
    "estimands_df"))
  )


})




test_that("Declare a bare function", {

  
  design <-
    declare_population(sleep) + 
    tidy_step(function(foo) foo)
  
  # design <- expect_warning(declare_design(
  #   sleep,
  #   function(foo) foo
  # ), "data")
  # 
  # expect_identical(draw_data(design), sleep)


})


