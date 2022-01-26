context("Multiple Coefficients")


test_that("Multiple Coefficients", {
  alpha <- 1
  beta <- 3

  my_pop <- declare_model(
    N = 30,
    noise = rnorm(N, mean = 0, sd = 1),
    X = 1:N,
    Y = alpha + beta * X + noise
  )

  theta <- declare_inquiry(
    `(Intercept)` = alpha,
    X = beta,
    term = TRUE
  )

  OLS <- declare_estimator(Y ~ X,
    model = lm,
    inquiry = theta,
    term = TRUE
  )

  my_design <- my_pop + theta + OLS
  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap_sims = FALSE)

  expect_equal(diagnosis %>% get_simulations %>% dim, c(4, 12))

  expect_equal(diagnosis %>% get_diagnosands %>% dim, c(2, 12))
})
