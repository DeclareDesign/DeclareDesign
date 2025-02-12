context("Design summary")

my_population <- declare_model(N = 500, noise = rnorm(N))

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_sampling <- declare_sampling(S = complete_rs(N, n = 250))

my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

test_that("Basic design summary w dplyr", {
  
  skip_if_not_installed("dplyr")
  
  design <- my_population +
    my_potential_outcomes +
    my_sampling +
    my_inquiry +
    declare_step(dplyr::mutate, q = 5) +
    my_assignment +
    my_measurement +
    my_estimator

  s <- summary(design)

  # First step
  expect_equal(s$N[[1]], "N = 500")
  expect_equal(s$call[[1]], attr(my_population, "call"))

  # Last step
  expect_equal(s$formulae[[8]], Y ~ Z)

  s_short <- summary(design, verbose = FALSE)

  expect_failure(expect_output(print(summary(design, verbose = FALSE)), "Formula"))
})

test_that("Basic design summary w/o dplyr", {
  
  design <- my_population +
    my_potential_outcomes +
    my_sampling +
    my_inquiry +
    my_assignment +
    my_measurement +
    my_estimator
  
  s <- summary(design)
  
  # First step
  expect_equal(s$N[[1]], "N = 500")
  expect_equal(s$call[[1]], attr(my_population, "call"))
  
  # Last step
  expect_equal(s$formulae[[7]], Y ~ Z)
  
  s_short <- summary(design, verbose = FALSE)
  
  expect_failure(expect_output(print(summary(design, verbose = FALSE)), "Formula"))
})


test_that("Add Quantitites and Alter Variables", {
  my_population <- declare_model(N = 500, noise = rnorm(N))
  my_inquiry <- declare_inquiry(foo = mean(noise))
  my_transform <- declare_model(noise = noise / 2)
  my_inquiry2 <- declare_inquiry(foo2 = mean(noise))


  design <- my_population +
    my_inquiry +
    my_transform +
    my_inquiry2

  # Adding Quantitites
  expect_output(
    print(design, verbose = TRUE), "A single draw of the"
  )

  # Altering variables
  expect_output(
    print(design, verbose = TRUE), "Altered variable: noise "
  )
})

test_that("str() works", {
  expect_output(str(declare_model(N = 50)), "design_step:\\t declare_model[(]N = 50[)] ")
})

test_that("summary, custom estimator handler, numeric value", {
  extra <- declare_estimator(
    handler = function(data)
      mean(data$extra)
  )
  d <- declare_model(sleep) + extra

  expect_output(print(d, verbose = TRUE), "1.54")
})

test_that("summary, estimator formula print formula", {
  extra <- declare_estimator(extra ~ group)
  d <- declare_model(sleep) + extra
  expect_output(print(d), "extra ~ group")
})

test_that("summary, estimator print model", {
  d <- declare_model(sleep) + declare_estimator(extra ~ group, .method = lm)
  expect_output(print(d, verbose = TRUE), "Method:\\s*lm")
})

