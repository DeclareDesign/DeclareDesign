context("Fanout execution")

testthat("Fanout does something",{

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
