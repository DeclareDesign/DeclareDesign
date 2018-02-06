context("Factorial Design")

test_that("Factorial", {

  alpha = 1
  beta = 3

  my_design <- declare_design(
    my_pop = declare_population(N=30, noise=rnorm(N, mean = 0, sd=1), X=1:N, Y = alpha + beta*X + noise)
    ,
    theta = declare_estimand(
      `(Intercept)`=alpha,
      X = beta,
      coefficient_names = TRUE
    )
    ,
    OLS = declare_estimator(Y~X, model=lm, estimand="theta", coefficient_name=NULL)
  )



  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

  expect_equal(diagnosis %>% get_simulations %>% dim, c(4, 10))

  expect_equal(diagnosis %>%  get_diagnosands %>% dim, c(2,11))

})
