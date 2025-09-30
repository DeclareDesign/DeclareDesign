context("Subsetting")

test_that("Test Subsetting on default inquiry handler", {
  my_population <- declare_model(N = 50, noise = rnorm(N))

  my_potential_outcomes <- declare_model(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(S = complete_rs(N, n = 25))

  my_inquiry <- declare_inquiry(
    ATE_pos = mean(Y_Z_1 - Y_Z_0),
    subset = Y_Z_1 > 0
  )

  my_inquiry2 <- declare_inquiry(
    ATE_neg = mean(Y_Z_1 - Y_Z_0),
    subset = Y_Z_1 < 0
  )



  design <- my_population + my_potential_outcomes + my_sampling + my_inquiry + my_inquiry2

  expect_true(design |> draw_estimands() |> with(estimand[1] > 2 && estimand[2] < 0))
  # > z <- replicate(10000, design  %>%  draw_estimands() %>% with(inquiry[[1]] > 2 && inquiry[2] < 0)) %>% table
  # > z
  # .
  # FALSE  TRUE
  # 8  9992
})
