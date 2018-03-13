context("Fanout execution")

test_that("Fanout does something",{

  N = 100

  pop <- declare_population(N=N)

  pop2 <- declare_population(noise=rnorm(N))

  estimand <- declare_estimand(foo=mean(noise))

  D <- declare_design(pop, pop2, estimand)

  fan_strategy <- data.frame(end=2:3, n=c(1,100))


  out <- DeclareDesign:::fan_out(D, fan_strategy)

  estimands_out <- do.call(rbind, lapply(out, `[[`, "estimands_df"))

  expect_equal( nrow(unique(estimands_out)), 1)

})


test_that("Diagnosing a fanout",{

  N = 100

  pop <- declare_population(N=N, noise=rnorm(N))

  estimand <- declare_estimand(foo=mean(noise))
  sampl <- declare_sampling(n=N/2)
  estimator <- declare_estimator(noise~1, model=lm, estimand=estimand, label="ha", coefficients=TRUE)


  D <- declare_design(pop, estimand, sampl, estimator)

  strategy <- c(1,1,5,20)

  dx <- diagnose_design(D, sims=strategy)

  # estimands don't vary overall
  expect_equal(
    dx$diagnosands[1,"se(mean_estimand)"], 0
  )

  rep_id <- setNames(rev(do.call(expand.grid, lapply(rev(strategy), seq))), names(D))

  expect_equivalent(
    tapply(dx$simulations$est, rep_id[dx$simulations$sim_ID, 3], var), c(0,0,0,0,0)
  )


})

