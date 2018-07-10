context("Assignment and probability functions")

test_that("randomizr works through declare_assignment", {
  df <- data.frame(ID = 1:10, blocks = rep(c("A", "B"), 5, 5))

  f_1 <- declare_assignment()
  expect_equal(sum(f_1(df)$Z), 5)

  f_1 <- declare_assignment(m = 5)
  expect_equal(sum(f_1(df)$Z), 5)

  f_1 <- declare_assignment(num_arms = 2)
  expect_equal(sum(f_1(df)$Z == "T1"), 5)

  f_1 <- declare_assignment(num_arms = 3)
  expect_true(all(table(f_1(df)$Z) >= 3))

  f_1 <- declare_assignment(blocks = blocks)
  expect_true(all.equal(
    unclass(xtabs(~blocks + Z, f_1(df))),
    matrix(c(3, 2, 3, 2), 2, 2), # slight bug in the blocks above with rep(AB,5,5) => ABABA x 2
    check.attributes = FALSE
  ))


  # what about inside a function?

  new_fun <- function(num_arms) {
    f_1 <- declare_assignment(num_arms = num_arms)
    f_1(df)
  }
  new_fun(3)
})




test_that("test assignment and probability functions", {
  population <- declare_population(
    villages = add_level(
      N = 100, elevation = rnorm(N),
      high_elevation = as.numeric(elevation > 0)
    ),
    individuals = add_level(
      N = 10, noise = rnorm(N),
      ideo_3 = sample(c("Liberal", "Moderate", "Conservative"),
        size = N, prob = c(.2, .3, .5), replace = TRUE
      )
    )
  )

  sampling <- declare_sampling(n = 10, clusters = villages)

  potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ 5 + .5 * (Z == 1) + .9 * (Z == 2) + .2 * Z * elevation + noise,
    conditions = c(0, 1, 2),
    assignment_variable = "Z"
  )



  smp_draw <- population() %>% sampling() %>% potential_outcomes()

  #  population() %>% sampling() %>% potential_outcomes()

  expect_assignment <- function(assn) {
    df <- assn(smp_draw)
    expect_true("Z_cond_prob" %in% names(df))
  }


  # Complete Random Assignment assignments
  assignment_0 <- declare_assignment() %>% expect_assignment() # blug
  assignment_1 <- declare_assignment(conditions = c(0, 1)) %>% expect_assignment()
  assignment_2 <- declare_assignment(m = 60, conditions = c(0, 1)) %>% expect_assignment()
  assignment_3 <- declare_assignment(m_each = c(20, 30, 50)) %>% expect_assignment()
  assignment_4 <- declare_assignment(m_each = c(20, 80), conditions = c(0, 1)) %>% expect_assignment()
  assignment_5 <- declare_assignment(prob_each = c(.2, .3, .5)) %>% expect_assignment()

  # Blocked assignments
  assignment_6 <- declare_assignment(blocks = ideo_3) %>% expect_assignment()
  assignment_7 <- declare_assignment(blocks = ideo_3, prob_each = c(.3, .6, .1)) %>% expect_assignment()
  assignment_8 <- declare_assignment(blocks = ideo_3, conditions = c(0, 1)) %>% expect_assignment()

  assignment_9 <- declare_assignment(
    blocks = ideo_3,
    conditions = c(0, 1),
    block_m = c(10, 10, 10)
  ) %>% expect_assignment()


  # Clustered assignments
  assignment_10 <- declare_assignment(clusters = villages) %>% expect_assignment()
  assignment_11 <- declare_assignment(clusters = villages, conditions = c(0, 1)) %>% expect_assignment()
  assignment_12 <- declare_assignment(clusters = villages, prob_each = c(.1, .3, .6)) %>% expect_assignment()

  # Blocked and Clustered assignments
  assignment_13 <- declare_assignment(
    clusters = villages,
    blocks = high_elevation
  ) %>% expect_assignment()

  assignment_14 <- declare_assignment(
    clusters = villages,
    blocks = high_elevation, conditions = c(0, 1)
  ) %>% expect_assignment()
  assignment_15 <- declare_assignment(
    clusters = villages,
    blocks = high_elevation, prob_each = c(.1, .3, .6)
  ) %>% expect_assignment()

  # Draw Data

  smp_draw %>% head()
})

test_that("more than 1 assignment", {
  assn <- declare_assignment(assignment_variable = P:Q)

  out <- assn(sleep)

  expect_equal(colnames(out), c("extra", "group", "ID", "P", "P_cond_prob", "Q", "Q_cond_prob"))
})


test_that("declare_assignment expected failures via validation fn", {
  expect_true(is.function(declare_assignment()))

  expect_error(declare_assignment(blocks = "character"), "blocks")

  expect_error(declare_assignment(clusters = "character"), "clusters")

  expect_error(declare_assignment(assignment_variable = NULL), "assignment_variable")
})


test_that("can append probabiliies matrix", {
  pop <- declare_population(N = 10)
  assignment <- declare_assignment(m = 5, append_probabilities_matrix = TRUE)
  dat <- draw_data(pop + assignment)

  expect_true("Z_prob_0" %in% colnames(dat))
})
