context("Fanout execution")

test_that("Fanout does something", {
  N <- 100

  pop <- declare_population(N = N)
  pop2 <- declare_step(fabricate, noise = rnorm(N))
  estimand <- declare_estimand(foo = mean(noise))
  D <- pop + pop2 + estimand

  fan_strategy <- data.frame(end = 2:3, n = c(1, 100))
  out <- DeclareDesign:::fan_out(D, fan_strategy)

  estimands_out <- do.call(rbind, lapply(out, `[[`, "estimands_df"))
  expect_equal(length(unique(estimands_out$estimand)), 1)
  expect_equal(estimands_out$step_1_draw, rep(1,100))
  expect_equal(estimands_out$step_3_draw, 1:100)
  
})

test_that("fanout should not be exposed to users", {
  N <- 100

  pop <- declare_population(N = N)
  pop2 <- declare_step(fabricate, noise = rnorm(N))
  estimand <- declare_estimand(foo = mean(noise))
  D <- pop + pop2 + estimand

  fan_strategy <- data.frame(end = 2:3, n = c(1, 100))
  expect_error(
    diagnose_design(D, sims = fan_strategy),
    "Please provide sims a scalar or a numeric vector of length the number of steps in designs."
  )

  expect_error(
    simulate_design(D, sims = fan_strategy),
    "Please provide sims a scalar or a numeric vector of length the number of steps in designs."
  )
})


test_that("Diagnosing a fanout", {
  N <- 100

  pop <- declare_population(N = N, noise = rnorm(N))

  estimand <- declare_estimand(foo = mean(noise))
  sampl <- declare_sampling(n = N / 2)
  estimator <-
    declare_estimator(
      noise ~ 1,
      model = lm,
      estimand = estimand,
      label = "ha",
      term = TRUE
    )


  D <- pop + estimand + sampl + estimator

  strategy <- c(1, 1, 5, 20)

  Sys.setenv(TESTTHAT='m')
  dx <- expect_warning(diagnose_design(D, sims = strategy))
  Sys.setenv(TESTTHAT='true')
  
  
  # estimands don't vary overall
  expect_equal(
    dx$diagnosands[1, "se(mean_estimand)"], 0
  )

  rep_id <-
    setNames(rev(do.call(expand.grid, lapply(rev(
      strategy
    ), seq))), names(D))

  expect_equivalent(tapply(dx$simulations$estimate, rep_id[dx$simulations$sim_ID, 3], var), c(0, 0, 0, 0, 0))

  expect_length(c("step_3_draw", "step_4_draw") %icn% dx$simulations, 2)
})

test_that("sims expansion is correct", {
  design <- declare_population(sleep) +
    declare_estimand(2, label = "a") +
    declare_estimand(b = rnorm(1))

  some_design <- declare_population(sleep) + declare_estimand(2, label = "a")
  some_design + declare_estimand(b = rnorm(1))

  sims <- c(1, 1, 1)
  expanded <- check_sims(design, sims)
  expect_equal(expanded$n, 1)

  sims <- 2
  expanded <- check_sims(design, sims)
  expect_equal(expanded$n, 2)

  sims <- c(a = 2)
  expanded <- check_sims(design, sims)
  expect_equal(expanded$n, c(1, 2))
})



test_that("fanout warnings", {
  N <- 100

  pop <- declare_population(N = N, noise = rnorm(N))

  estimand <- declare_estimand(foo = mean(noise))
  sampl <- declare_sampling(n = N / 2)
  estimator <-
    declare_estimator(
      noise ~ 1,
      model = lm,
      estimand = estimand,
      label = "ha",
      term = TRUE
    )


  D <- pop + estimand + sampl + estimator

  strategy <- c(1, 1, 1, 1)

  Sys.setenv(TESTTHAT='m')
  expect_warning(diagnose_design(D, sims = strategy))
  Sys.setenv(TESTTHAT='true')
  
})


test_that("correct fan out", {
  f1 <- local({
    i <- 0
    function() {
      i <<- i + 1
      i
    }
  })
  f2 <- local({
    i <- 0
    function() {
      i <<- i + 1
      i
    }
  })
  f3 <- local({
    i <- 0
    function() {
      i <<- i + 1
      i
    }
  })
  e1 <- declare_estimand(a = f1())
  e2 <- declare_estimand(b = f2())
  e3 <- declare_estimand(c = f3())
  
  out <-
    simulate_design(declare_population(sleep) + e1 + e2 + e3, sims = c(30, 1, 5, 2))
  
  expect_equivalent(apply(out[,c(5:7)], 2, max), c(30, 150, 300))
  expect_equivalent(tapply(out$estimand, INDEX = out$estimand_label, max), c(30, 150, 300))
  
})



