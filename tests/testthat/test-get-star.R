

context("get_ functions")

population <- declare_population(N = 100, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = 0,
                                                 Y_Z_1 = 1 + u)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
sampling <- declare_sampling(n = 75)
assignment <- declare_assignment(m = 50)
reveal_Y <- declare_reveal(Y, Z)
estimator <- declare_estimator(Y ~ Z, estimand = estimand)
design <-
  population + potential_outcomes + estimand + sampling + assignment + reveal_Y + estimator

dat <- draw_data(design)
dat$Z <- NULL
dat$Z_cond_prob <- NULL

test_that("get_ works", {
  dat_with_Z <- draw_assignment(design, dat)
  
  expect_true(all(c("Z", "Z_cond_prob") %in% colnames(dat_with_Z)))
  
  dat_sampled <- draw_sample(design, dat[c(1:75, 1:75), ])
  expect_equal(nrow(dat_sampled), 75)
  
})