context("Continuous POs")
test_that("you can do continuous POs", {
  my_population <- declare_population(
    N = 100, income = rnorm(N), age = sample(18:95, N, replace = T)
  )

  conditions <- seq(0, 1, by = .1)

  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z, conditions = conditions
  )

  my_assignment <- declare_assignment(conditions = conditions)

  my_reveal <- reveal_outcomes()

  my_design <- my_population +
    my_potential_outcomes +
    my_assignment +
    my_reveal

  df <- head(draw_data(my_design))

  expect_length(colnames(df) %i% paste("Y", "Z", conditions, sep = "_"), 11)
})

test_that("Hooke's law", {

  # Length of spring = resting length + stiffness * Force

  # Length is cm
  # Force is g
  # stiffness is cm/g

  # Variablity in manufacturing
  pop <-
    declare_population(springs = add_level(
      N = 100,
      resting = rnorm(N, 10, .1),
      stiffness = rnorm(N, 1, .05)
    ))


  potential_outcome_f <-
    function(resting, stiffness, force) {
      resting + stiffness * force
    }

  estimand <- declare_estimand(
    `(Intercept)` = mean(potential_outcome_f(resting, stiffness, 0)),
    stiffness = mean(potential_outcome_f(resting, stiffness, 1) - potential_outcome_f(resting, stiffness, 0)),
    term = TRUE
  )

  # 30 is magic
  sampling <- declare_sampling(n = 30)

  # We don't have a 1g weight, only 5, 10, 25, 50, 100
  # randomly put a combo of those on the spring
  w <- c(0, 5, 10, 25, 50, 100)

  assignment <- declare_assignment(
    handler = fabricate,
    force = replicate(N, sum(sample(
      w, sample(length(w), 1)
    )))
  )

  # 1mm of measurment error
  reveal <- reveal_outcomes(
    handler = fabricate,
    length = potential_outcome_f(resting, stiffness, force) +
      rnorm(N, sd = .1)
  )

  estimator <- declare_estimator(length ~ force, model = lm, term = TRUE)

  design <- pop + estimand + sampling + assignment + reveal + estimator

  df <- draw_data(design)

  # Not all forces are realized
  expect_lt(length(unique(df$force)), sum(choose(length(w), seq_along(w))))

  # No PO columns created in df
  expect_false(any(grep("length_force_", names(df))))
})
