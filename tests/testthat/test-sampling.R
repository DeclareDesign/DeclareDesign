context("Sampling and probability functions")

test_that("use of randomizr and filter works", {
  
  design <- declare_model(
    classrooms = add_level(10),
    individuals = add_level(20, female = rbinom(N, 1, 0.5))
  ) + NULL
  
  dat <- draw_data(design)
  
  smp1 <- declare_sampling(handler = sampling_handler, S = complete_rs(N = N, n = 10), filter = S == 1)
  smp2 <- declare_sampling(handler = sampling_handler, S = complete_rs(N = N, n = 10))
  smp3 <- declare_sampling(handler = sampling_handler, S = complete_rs(N = N, n = 10), filter = S == 0)
  
  expect_equal(nrow(smp1(dat)), 10)
  expect_equal(nrow(smp2(dat)), 10)
  expect_equal(nrow(smp3(dat)), 190)
  
})


test_that("legacy warnings", {
  expect_error(declare_sampling(handler = sampling_handler, n = 50), "S = draw_rs\\(N = N, n = 50\\)")
  expect_error(declare_sampling(handler = sampling_handler, n = 50, sampling_variable = "D"), "D = draw_rs\\(N = N, n = 50\\)")
  expect_silent(declare_sampling(handler = sampling_handler, S = complete_rs(N = N, n = 20)))
})
