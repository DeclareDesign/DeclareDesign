context("Save-Reload-Redesign")

test_that("Save Reload Redesign works", {
  N <- 100
  d <- declare_model(N = N, foo = rnorm(N)) + declare_inquiry(d = mean(foo))
  f <- tempfile()

  suppressWarnings(
    saveRDS(d, f)
  )

  rm(d, N)

  suppressWarnings(
    d <- readRDS(f)
  )

  expect_true(is.data.frame(draw_data(d)))

  d2 <- redesign(d, N = 20)

  expect_true(is.data.frame(draw_data(d2)))
})

test_that("Save Reload Redesign works with lm_robust", {
  library(testthat)
  library(DeclareDesign)
  library(tidyverse)
  N <- 100
  d <- declare_model(N = N, foo = rnorm(N)) + declare_inquiry(d = mean(foo)) + declare_estimator(foo ~ 1, model = lm_robust)
  f <- "~/desktop/tmp.RDS"
  
  suppressWarnings(
    write_rds(d, file = f)
  )
  
  rm(d, N)
  
  suppressWarnings(
    d <- readRDS(f)
  )
  
  expect_true(is.data.frame(run_design(d)))
  
  d2 <- redesign(d, N = 20)
  
  expect_true(is.data.frame(run_design(d2)))
})
