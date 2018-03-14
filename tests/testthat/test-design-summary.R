context("Design summary")

test_that("Basic design summary", {

  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(n = 250)

  my_assignment <- declare_assignment(m = 25)

  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

  reveal_outcomes <- declare_reveal()

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           my_sampling,
                           my_estimand,
                           dplyr::mutate(q = 5),
                           my_assignment,
                           reveal_outcomes,
                           my_estimator)

  s <- summary(design)

  # First step
  expect_equal(s$N[[1]], "N = 500")
  expect_equal(s$call[[1]], attr(my_population, "call"))

  # Last step
  expect_equal(s$formulae[[8]], Y~Z)


})


test_that("Add Quantitites and Alter Variables", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_estimand   <- declare_estimand(foo = mean(noise))
  my_transform  <- declare_population(noise = noise / 2)
  my_estimand2  <- declare_estimand(foo2 = mean(noise))


  design <- declare_design(my_population,
                           my_estimand,
                           my_transform,
                           my_estimand2)

  # Adding Quantitites
  expect_output(
    print(design), "A single draw of the"
  )

  # Altering variables
  expect_output(
    print(design), "Altered variable: noise "
  )
})

test_that("str() works", {

  expect_output(str(declare_population(N = 50)), "design_step:\\t declare_population[(]N = 50[)] ")
  expect_output(str(declare_design(sleep)), "design_step:\\t sleep")

})

test_that("summary, custom estimator handler, numeric value", {
      d <- declare_design(sleep, extra=declare_estimator(handler=function(data) mean(data$extra)))

      expect_output(print(d), "1.54")

})

test_that("summary, estimator formula print formula", {
  d <- declare_design(sleep, extra=declare_estimator(extra~group))

  expect_output(print(d), "extra ~ group")

})

