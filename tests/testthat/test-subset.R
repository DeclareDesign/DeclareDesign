context("Subsetting")

test_that("Test Subsetting on default estimand handler", {
  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_estimand <- declare_estimand(
    ATE_pos = mean(Y_Z_1 - Y_Z_0),
    subset = Y_Z_1 > 0
  )

  my_estimand2 <- declare_estimand(
    ATE_neg = mean(Y_Z_1 - Y_Z_0),
    subset = Y_Z_1 < 0
  )



  design <- my_population + my_potential_outcomes + my_sampling + my_estimand + my_estimand2

  expect_true(design %>% draw_estimands() %>% with(estimand[1] > 2 && estimand[2] < 0))
  # > z <- replicate(10000, design  %>%  draw_estimands() %>% with(estimand[[1]] > 2 && estimand[2] < 0)) %>% table
  # > z
  # .
  # FALSE  TRUE
  # 8  9992
})
