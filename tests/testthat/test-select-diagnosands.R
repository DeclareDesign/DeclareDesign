context("select diagnosands")

test_that("select diagnosands works", {

  design <- 
    declare_model(N = 100, u = rnorm(N),
                  Y_Z_0 = 0, 
                  Y_Z_1 = ifelse(rbinom(N, 1, prob = 0.5), 0.1, -0.1) + u
    ) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")

  diagnosands_1 <- select_diagnosands("bias", "power")
  diagnosands_2 <- select_diagnosands("bias", "power", alpha = .000001)
  
  expect_true("design_step" %in% class(diagnosands_1))
  sims <- simulate_design(design, sims = 20)
  expect_true(all(dim(diagnosands_1(sims)) == c(2,2))) 
  
  # alpha argument works
  expect_true(
    diagnosands_1(sims)$diagnosand[2] > diagnosands_2(sims)$diagnosand[2])
   
  # error works
  expect_message(select_diagnosands("artichokes"))
  expect_true(is.null(select_diagnosands("artichokes")))

  # NA works
    x <-
      select_diagnosands("bias", na.rm = TRUE)(data.frame(estimate = 1:2, estimand = c(NA, 2)))
  
  expect_true(!is.na(x$diagnosand[1]))

  
  })



