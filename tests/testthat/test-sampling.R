context("Sampling and probability functions")





test_that("use of randomizr and filter works", {
  
  design <- declare_model(
    classrooms = add_level(10),
    individuals = add_level(20, female = rbinom(N, 1, 0.5))
  ) + NULL
  
  dat <- draw_data(design)
  
  smp1 <- declare_sampling(S = complete_rs(N = N, n = 10), filter = S == 1)
  smp2 <- declare_sampling(S = complete_rs(N = N, n = 10))
  smp3 <- declare_sampling(S = complete_rs(N = N, n = 10), filter = S == 0)
  smp4 <- declare_sampling(S = sample(x = c(0, 1, NA), N, replace = TRUE), filter = S == 0)
  
  expect_equal(nrow(smp1(dat)), 10)
  expect_equal(nrow(smp2(dat)), 10)
  expect_equal(nrow(smp3(dat)), 190)
  expect_true(sum(is.na(smp4(dat)$S)) == 0)
})


test_that("legacy warnings", {
  expect_error(declare_sampling(n = 50), "S = draw_rs\\(N = N, n = 50\\)")
  expect_error(declare_sampling(n = 50, sampling_variable = "D"), "D = draw_rs\\(N = N, n = 50\\)")
  expect_silent(declare_sampling(S = complete_rs(N = N, n = 20)))
})



context("Sampling and probability functions")

test_that("randomizr works through declare_sampling", {
  df <- data.frame(ID = 1:10, strata = rep(c("A", "B"), 5, 5))
  
  f_1 <- declare_sampling(legacy = TRUE)
  expect_equal(dim(f_1(df)), c(5, 3))
  
  f_1 <- declare_sampling(n = 4, legacy = TRUE)
  expect_equal(dim(f_1(df)), c(4, 3))
  
  f_1 <- declare_sampling(strata = strata, legacy = TRUE)
  expect_length(xtabs(~strata, f_1(df)), 2)
  
  
  # what about inside a function?
  
  new_fun <- function(n) {
    f_1 <- declare_sampling(n = n, legacy = TRUE)
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
  expect_sampling_step(declare_sampling(legacy = TRUE), population, n = 500)
  expect_sampling_step(declare_sampling(legacy = TRUE, n = 60), population, n = 60)
  
  # stratified sampling
  expect_sampling_step(declare_sampling(legacy = TRUE, strata = ideo_3), population, n = NA)
  expect_sampling_step(declare_sampling(legacy = TRUE, strata = ideo_3, strata_prob = c(.3, .6, .1)), population, n = NA)
  expect_sampling_step(declare_sampling(legacy = TRUE, strata = ideo_3, strata_n = c(10, 10, 10)), population, n = 30)
  
  # Clustered sampling
  expect_sampling_step(declare_sampling(legacy = TRUE, clusters = villages), population, n = 500)
  
  # Stratified and Clustered assignments
  expect_sampling_step(declare_sampling(legacy = TRUE, clusters = villages, strata = high_elevation), population, n = NA)
})


test_that("declare_sampling expected failures via validation fn", {
  expect_true(is.function(declare_sampling(legacy = TRUE)))
  
  expect_error(declare_sampling(strata = "character", legacy = TRUE), "strata")
  
  expect_error(declare_sampling(clusters = "character", legacy = TRUE), "clusters")
  
  expect_error(declare_sampling(sampling_variable = NULL, legacy = TRUE), "sampling_variable")
})






# two by two: keep/drop standard name/non standard name

test_that("keep/drop options work with diff sampling names", {
  
  desgn <- declare_population(N = 10) + NULL 
  
  dat1 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5))
  dat2 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5, drop_nonsampled = TRUE))
  dat3 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5, drop_nonsampled = FALSE))
  dat4 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5, sampling_variable = "smpld"))
  dat5 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5, sampling_variable = "smpld", drop_nonsampled = TRUE))
  dat6 <- draw_data(desgn + declare_sampling(legacy = TRUE, n = 5, sampling_variable = "smpld", drop_nonsampled = FALSE))
  
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
