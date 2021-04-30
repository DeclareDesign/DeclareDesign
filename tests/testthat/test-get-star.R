

context("get_ functions")

population <- declare_population(N = 100, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = 0,
                                                 Y_Z_1 = 1 + u)
inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
sampling <- declare_sampling(legacy = FALSE, S = complete_rs(N, n = 75))
assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 50))
reveal_Y <- declare_reveal(Y, Z)
estimator <- declare_estimator(Y ~ Z, inquiry = inquiry)
design <-
  population + potential_outcomes + inquiry + sampling + assignment + reveal_Y + estimator

dat <- draw_data(design)
dat$Z <- NULL

test_that("error when send list of designs to draw_data", {
  
  expect_error(draw_data(list(design, design)), "Please send a single design object to the design argument, typically created using the \\+ operator.")
  
})

test_that("get_ works", {
  dat_with_Z <- draw_assignment(design, dat)
  
  expect_true(all(c("Z") %in% colnames(dat_with_Z)))
  
  dat_sampled <- draw_sample(design, dat[c(1:75, 1:75), ])
  expect_equal(nrow(dat_sampled), 75)
  
})


test_that("no sampling does not produce error from draw_sample", {
  
  design <- population + potential_outcomes + inquiry
  
  dat <- draw_data(design)
  
  expect_error(draw_sample(design, data = dat), "does not include any sampling steps")
  
  expect_error(draw_assignment(design, data = dat), "does not include any assignment steps")
  
})

