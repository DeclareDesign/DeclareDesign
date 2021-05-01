context("design library")


test_that("design library dependency works",{
  skip_if_not_installed("DesignLibrary")
  skip_on_cran()
  design_1 <- DesignLibrary::two_by_two_designer(N = 500, outcome_means = c(0,0,1,2), weight_A = 0, weight_B = 0)
  design_2 <- DesignLibrary::multi_arm_designer(N = 500, m_arms = 3, outcome_means = c(0, 0, 1))
  dx <- diagnose_design(design_1, design_2, sims = 3, bootstrap_sims = FALSE)
  
  expect_true(all(c("design_1", "design_2") %in% dx$diagnosands_df$design))
  
})



# fan out

test_that("MH sim ids", {
  skip_if_not_installed("DesignLibrary")
  skip_on_cran()
  design <- DesignLibrary::two_arm_designer()
  
  Sys.setenv(TESTTHAT='m')
  sx <- expect_warning(simulate_design(design, sims = c(2, 1, 1, 1, 1, 2)))
  Sys.setenv(TESTTHAT='true')
  
  expect_equal(sx$step_1_draw, c(1L, 1L, 2L, 2L))
  expect_equal(sx$step_6_draw, c(1L, 2L, 3L, 4L))
  expect_equal(sx$estimate[1], sx$estimate[2])
  expect_equal(sx$estimate[3], sx$estimate[4])
})


test_that("fan out IDs are correct", {
  
  skip_if_not_installed("DesignLibrary")
  skip_on_cran()
  
  sims <- c(30, 1, 2, 1, 1, 2)
  design <- DesignLibrary::two_arm_designer(rho = 0)
  
  sx <- simulate_design(design, sims = sims)
  
  expect_equivalent(vapply(sx[c("step_1_draw", "step_3_draw", "step_6_draw")], max, 0), c(30, 60, 120))
})