test_that("test the estimands", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  ## default labeling
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  my_population() %>% my_potential_outcomes %>% my_estimand


  ## no label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  my_population() %>% my_potential_outcomes %>% my_estimand

  ## manual label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = ATE)
  my_population() %>% my_potential_outcomes %>% my_estimand

  ## custom estimand function
  my_estimand_function <- function(data) {
    with(data, median(Y_Z_1 - Y_Z_0))
  }
  my_estimand_custom <- declare_estimand(
    estimand_function = my_estimand_function, label = medianTE)

  my_population() %>% my_potential_outcomes %>% my_estimand_custom

})
