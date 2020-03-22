context("Sampling and probability functions")

test_that("randomizr works through declare_sampling", {
  df <- data.frame(ID = 1:10, strata = rep(c("A", "B"), 5, 5))

  f_1 <- declare_sampling()
  expect_equal(dim(f_1(df)), c(5, 3))

  f_1 <- declare_sampling(n = 4)
  expect_equal(dim(f_1(df)), c(4, 3))

  f_1 <- declare_sampling(strata = strata)
  expect_length(xtabs(~strata, f_1(df)), 2)


  # what about inside a function?

  new_fun <- function(n) {
    f_1 <- declare_sampling(n = n)
    f_1(df)
  }
  expect_equal(dim(new_fun(3)), c(3, 3))
})

expect_sampling_step <- function(step, df, n, clusters = NULL, strata = NULL) {
  df <- step(df)

  if (!is.na(n)) {
    expect_equal(nrow(df), n)
  }

  expect_true(is.numeric(df$S_inclusion_prob))

  if (is.character(clusters)) {

  }
  if (is.character(strata)) {

  }
}

test_that("test sampling and probability functions", {
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
  # Draw Data
  population <- population()


  # "complete" sampling
  expect_sampling_step(declare_sampling(), population, n = 500)
  expect_sampling_step(declare_sampling(n = 60), population, n = 60)

  # stratified sampling
  expect_sampling_step(declare_sampling(strata = ideo_3), population, n = NA)
  expect_sampling_step(declare_sampling(strata = ideo_3, strata_prob = c(.3, .6, .1)), population, n = NA)
  expect_sampling_step(declare_sampling(strata = ideo_3, strata_n = c(10, 10, 10)), population, n = 30)

  # Clustered sampling
  expect_sampling_step(declare_sampling(clusters = villages), population, n = 500)

  # Stratified and Clustered assignments
  expect_sampling_step(declare_sampling(clusters = villages, strata = high_elevation), population, n = NA)
})


test_that("declare_sampling expected failures via validation fn", {
  expect_true(is.function(declare_sampling()))

  expect_error(declare_sampling(strata = "character"), "strata")

  expect_error(declare_sampling(clusters = "character"), "clusters")

  expect_error(declare_sampling(sampling_variable = NULL), "sampling_variable")
})






test_that("Factor out declarations", {
  skip_if_not_installed("randomizr", "0.15.0")

  N <- 500
  n <- 250
  m <- 25

  expect_message(
    design <- declare_population(N = N, noise = 1:N) + declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 1) + declare_sampling(N = N, n = n) + declare_assignment(N = n, m = m) + declare_reveal(),
    "declaration"
  )

  expect_true(inherits(attr(design[[3]], "dots")$declaration, "rs_complete"))
  expect_true(inherits(attr(design[[4]], "dots")$declaration, "ra_complete"))
})

# two by two: keep/drop standard name/non standard name

test_that("keep/drop options work with diff sampling names", {
  
  desgn <- declare_population(N = 10) + NULL 
  
  dat1 <- draw_data(desgn + declare_sampling(n = 5))
  dat2 <- draw_data(desgn + declare_sampling(n = 5, drop_nonsampled = TRUE))
  dat3 <- draw_data(desgn + declare_sampling(n = 5, drop_nonsampled = FALSE))
  dat4 <- draw_data(desgn + declare_sampling(n = 5, sampling_variable = "smpld"))
  dat5 <- draw_data(desgn + declare_sampling(n = 5, sampling_variable = "smpld", drop_nonsampled = TRUE))
  dat6 <- draw_data(desgn + declare_sampling(n = 5, sampling_variable = "smpld", drop_nonsampled = FALSE))
  
  # length, which variables
  expect_equal(nrow(dat1), 5)
  expect_equal(nrow(dat2), 5)
  expect_equal(nrow(dat3), 10)
  expect_false("S" %in% names(dat1))
  expect_false("S" %in% names(dat2))
  expect_true("S" %in% names(dat3))
  
  expect_equal(nrow(dat4), 5)
  expect_equal(nrow(dat5), 5)
  expect_equal(nrow(dat6), 10)
  expect_false("smpld" %in% names(dat4))
  expect_false("smpld" %in% names(dat5))
  expect_true("smpld" %in% names(dat6))
  
})
