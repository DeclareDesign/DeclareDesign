

test_that("allow design functions to be sent to simulate design and diagnose_design", {
  
  my_design_function <- function(){
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
      declare_step(dplyr::mutate, q = 5) +
      my_assignment +
      my_reveal +
      my_estimator
    
    run_design(design)
  }
  
  des_out <- run_design(my_design_function)
  
  expect_equal(names(des_out), c("estimates_df", "estimands_df"))
  
  sims_out <- simulate_design(my_design_function, sims = 2)
  
  expect_equal(sims_out[, 1:2], 
               structure(list(design_label = c("my_design_function", "my_design_function"
               ), sim_ID = 1:2), class = "data.frame", row.names = c(NA, -2L
               )))
  
  diag_out <- diagnose_design(my_design_function, sims = 2)
  
  expect_equal(diag_out$diagnosands_df[, 1:4], 
               structure(list(design_label = structure(1L, .Label = "my_design_function", class = "factor"), 
                              estimand_label = "ATE", estimator_label = "my_estimator", 
                              coefficient = "Z"), class = "data.frame", row.names = c(NA, 
                                                                                      -1L)))
})


test_that("error when you send other objects to diagnose", {
  
  # must send a function or a design object
  expect_error(diagnose_design(rep(3, 2)), "Please only send design objects or functions with no arguments to simulate_design.")
  
})