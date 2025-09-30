
context("design library")

library(rlang)

test_that("Design library basic", {
  testthat::skip("test requires updated DesignLbrary")    
  skip_if_not_installed("DesignLibrary")
  
  q = .6
  design <- DesignLibrary::two_arm_covariate_designer(prob = q, N = 15)
  design[[1]]
  expect_true(draw_data(design) |> nrow() == 15)
  
  expect_true(
    all(DesignLibrary::factorial_designer(N = 12) |> draw_data() |> dim() == c(12,15))
  )
  
  expect_true(
    all(DesignLibrary::binary_iv_designer(N = 12) |> draw_data() |> dim() == c(12,10))
  )

  design_22 <- DesignLibrary::two_by_two_designer(
    N = 10, outcome_means = c(0,0,1,2), weight_A = 0, weight_B = 0)

  expect_true(draw_data(design_22) |> ncol() == 9)

  design <- DesignLibrary::multi_arm_designer(
    N = 10, m_arms = 3, outcome_means = c(0, 0, 1))
  expect_true(draw_data(design) |> nrow() == 10)
  
})

# Errors thrown when N argument provided to designer inside test environment
test_that("Design library error with N from test env", {
  skip_if_not_installed("DesignLibrary")
  testthat::skip("test requires updated DesignLbrary")    
  
    n <- 20
  expect_error(design <- DesignLibrary::two_arm_covariate_designer(N = n))
})


test_that("Design error addressed using do.call  with N from test env", {
  skip_if_not_installed("DesignLibrary")
  testthat::skip("test requires updated DesignLbrary")    
  n <- 20
  design <- do.call(DesignLibrary::two_arm_covariate_designer,
                    list(N = n))
  expect_equal(nrow(draw_data(design)), 20L)
})


test_that("multiarm by hand",{
  
# Parameters
N <- 10
outcome_means <- c(0, 0, 1)
sd_i <- 1
outcome_sds <- c(0, 0, 0)

estimator_handler <- 
  function(data) {
  estimates <- rbind.data.frame(
    ate_Y_2_1 = difference_in_means(Y ~ Z, data = data, condition1 = "1", condition2 = "2"),
    ate_Y_3_1 = difference_in_means(Y ~ Z, data = data, condition1 = "1", condition2 = "3"),
    ate_Y_3_2 = difference_in_means(Y ~ Z, data = data, condition1 = "2", condition2 = "3")
  )
  names(estimates)[names(estimates) == "N"] <- "N_DIM"
  estimates$estimator <- c("DIM (Z_2 - Z_1)", "DIM (Z_3 - Z_1)", "DIM (Z_3 - Z_2)")
  estimates$inquiry <- rownames(estimates)
  estimates$estimate <- estimates$coefficients
  estimates$term <- NULL
  return(estimates)
}

# Declare design steps
population <- declare_population(
  N = N,
  u_1 = rnorm(N, 0, outcome_sds[1L]),
  u_2 = rnorm(N, 0, outcome_sds[2L]),
  u_3 = rnorm(N, 0, outcome_sds[3L]),
  u = rnorm(N) * sd_i
)

potentials <- declare_potential_outcomes(
  formula = Y ~ (outcome_means[1] + u_1) * (Z == "1") +
    (outcome_means[2] + u_2) * (Z == "2") +
    (outcome_means[3] + u_3) * (Z == "3") + u,
  conditions = c("1", "2", "3"),
  assignment_variables = Z
)

estimand <- declare_inquiries(
  ate_Y_2_1 = mean(Y_Z_2 - Y_Z_1),
  ate_Y_3_1 = mean(Y_Z_3 - Y_Z_1),
  ate_Y_3_2 = mean(Y_Z_3 - Y_Z_2)
)

assignment <- declare_assignment(
  Z = complete_ra(N, num_arms = 3, conditions = c("1", "2", "3"))
)

reveal_Y <- declare_reveal(assignment_variables = Z)

estimator <- declare_estimator(handler = estimator_handler)

# Combine design
multi_arm_design <- 
  population +
  potentials +
  assignment +
  reveal_Y +
  estimand +
  estimator
multi_arm_design

multi_arm_design_2 <-
  redesign(multi_arm_design, 
         N = 8, 
         outcome_sds = list(c(1,0,1)), 
         sd_i = .5, 
         outcome_means = list(c(2, 0, 1))
         )

expect_true(find_all_objects(multi_arm_design)$value_str[1] == "10")
expect_true(find_all_objects(multi_arm_design_2)$value_str[1] == "8")

})




test_that("design library dependency works",{
  skip_if_not_installed("DesignLibrary")
  testthat::skip("test requires updated DesignLbrary")    
    skip_on_cran()

  outcome_means = 1:3

  design_2 <- DesignLibrary::multi_arm_designer(
    N = 10, m_arms = 3, outcome_means = c(0, 0, 1))

  df <- design_2[[1]]()
  po_lib <- design_2[[2]]
  po <- declare_potential_outcomes(
    formula = Y ~ 
      (outcome_means[1] + u_1) * (Z == "1") + 
      (outcome_means[2] + u_2) * (Z == "2") + 
      (outcome_means[3] + u_3) * (Z == "3") + u, 
    conditions = c("1", "2", "3"), assignment_variables = Z) 

  expect_true(all((po(df) |> dim()) == dim(po_lib(df))))

})


