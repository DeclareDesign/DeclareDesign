
test_that("get_estimates_data works", {
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
    my_estimator + 
    declare_estimator(Y ~ Z, model = lm_robust, label = "est2")
  
  get_estimates_data(design, data = draw_data(design))
  
  get_estimates_data(design, data = draw_data(design), start = 9)
  
})