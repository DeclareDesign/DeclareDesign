test_that("test the estimators", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  my_assignment <- declare_assignment(m = 25)

  ## difference in means
  my_estimator <- declare_estimator(Y ~ Z)
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## lm with robust ses
  my_estimator <- declare_estimator(Y ~ Z, estimator_function = lm_robust_se)
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## lm with HC3 robust ses
  my_estimator <- declare_estimator(Y ~ Z, estimator_function = lm_robust_se, se_type = "HC3")
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## custom estimator function
  my_estimator_function <- function(formula, data){
    data.frame(est = with(data, mean(Y)))
  }

  my_estimator_custom <-
    declare_estimator(Y ~ Z, estimator_function = my_estimator_function)

  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator_custom


  ## check blocked d-i-m estimator

  my_population <- declare_population(N = 500, noise = rnorm(N), block_var = sample(rep(c("A", "B"), each = 250), N, replace = F))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2) + 5 * (block_var == "A"))
  my_assignment <- declare_assignment(block_var = block_var)

  ## lm with HC3 robust ses
  my_estimator_blocked <- declare_estimator(Y ~ Z, estimator_function = difference_in_means_blocked, block_variable_name = block_var)
  df <- my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes
  my_estimator_notblocked <- declare_estimator(Y ~ Z)

  df %>% my_estimator_notblocked
  df %>% my_estimator_blocked ## it is different!

})
