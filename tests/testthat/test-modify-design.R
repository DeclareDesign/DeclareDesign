context("modify design")

test_that("test modify declare design ", {
  library(dplyr)
  N <- 500

  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25), label = "a_label")

  design <- my_population +
    my_potential_outcomes +
    declare_step(dplyr::mutate, q = 5) +
    my_assignment

  my_assignment_2 <- declare_assignment(Z2 = complete_ra(N, m = 25))

  expect_length(replace_step(design, new_step = my_assignment_2, step = my_assignment), 4)
  expect_length(replace_step(design, new_step = my_assignment_2, step = 4), 4)
  expect_length(replace_step(design, new_step = my_assignment_2, step = "a_label"), 4)

  redesigned <- replace_step(design, new_step = my_assignment_2, step = my_assignment)

  expect_equal(names(redesigned), c("my_population", "my_potential_outcomes", "custom", "my_assignment_2"))

  expect_length(insert_step(design, declare_step(mutate, blah = 6), before = my_potential_outcomes), 5)

  expect_length(insert_step(design, declare_step(mutate, blah = 6), after = my_potential_outcomes), 5)

  expect_length(replace_step(design, declare_step(mutate, blah = 10), step = my_population), 4)

  expect_length(delete_step(design, 3), 3)
})



test_that("placement doesn't matter", {
  my_population <-
    declare_population(
      N = 100,
      noise = rnorm(N),
      label = "mypop"
    )

  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = noise,
      Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
    )

  my_assignment <- declare_assignment(Z = complete_ra(N, m = 50))
  my_assignment_2 <- declare_assignment(Z = complete_ra(N, m = 25))

  design <- my_population + my_potential_outcomes + my_assignment

  expect_length(insert_step(design, declare_step(mutate, income = noise^2), after = my_assignment), 4)
  expect_length(insert_step(design, declare_step(mutate, income = noise^2), before = my_assignment), 4)

  expect_length(insert_step(design, declare_step(mutate, income = noise^2), before = "mypop"), 4)

  expect_error(insert_step(design, declare_step(mutate, income = noise^2), before = "notfound"))
  expect_error(insert_step(design, declare_step(mutate, income = noise^2)))
})


test_that("names are correct", {
  my_population <- declare_population(N = 100, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25), label = "a_label")

  design <- my_population +
    my_potential_outcomes +
    my_assignment +
    declare_reveal()


  processing <- declare_step(handler = fabricate, q = 5)

  linear <- declare_estimator(Y ~ Z,
    model = lm_robust,
    label = "linear"
  )

  saturated <- declare_estimator(Y ~ Z, label = "saturated", model = lm_robust)

  neighbors_design <- insert_step(design, after = 3, processing)

  expect_equal(
    names(neighbors_design),
    c("my_population", "my_potential_outcomes", "a_label", "processing", "reveal")
  )

  check0 <- neighbors_design + linear
  check1 <- neighbors_design + saturated
  check2 <- neighbors_design + linear + saturated

  expect_equal(
    names(check0),
    c(
      "my_population", "my_potential_outcomes", "a_label", "processing",
      "reveal", "linear"
    )
  )

  expect_equal(
    names(check1),
    c(
      "my_population", "my_potential_outcomes", "a_label", "processing",
      "reveal", "saturated"
    )
  )
  expect_equal(
    names(check2),
    c(
      "my_population", "my_potential_outcomes", "a_label", "processing",
      "reveal", "linear", "saturated"
    )
  )
})
