
context("design library")

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

DeclareDesign:::find_all_objects(multi_arm_design)
DeclareDesign:::find_all_objects(multi_arm_design_2)

})


# To Do: These should not have to be saved
u_1 <- u_2 <-  u_3 <- u <- 1

test_that("design library dependency works",{
  skip_if_not_installed("DesignLibrary")
  skip_on_cran()

  # Factorial designer  
  expect_true(
    all(DesignLibrary::factorial_designer(N = 12) |> draw_data() |> dim() == c(12,15))
    )
    
  expect_true(
    all(DesignLibrary::binary_iv_designer(N = 12) |> draw_data() |> dim() == c(12,10))
    )
  
  # flag: should not be needed; this due to enquo call
  library(rlang)
  
  design_1 <- DesignLibrary::two_by_two_designer(
    N = 10, outcome_means = c(0,0,1,2), weight_A = 0, weight_B = 0)
  
  design_2 <- DesignLibrary::multi_arm_designer(
    N = 10, m_arms = 3, outcome_means = c(0, 0, 1))

  design_1 |> draw_data()  
  
  # To Do: These should not have to be saved
  # Flag: works on own but not inside testhat environment
  
  u_1 <- u_2 <-  u_3 <- u <- 1
  design_2 |> draw_data()
  dx <- diagnose_design(design_1, design_2, sims = 3, 
                        bootstrap_sims = FALSE)
  
  DesignLibrary::get_design_code(design_2)
  expect_true(all(c("design_1", "design_2") %in% dx$diagnosands_df$design))

  
  DeclareDesign:::find_all_objects(design_2)  
  
  library(DesignLibrary)
  design_2 <- DesignLibrary::multi_arm_designer(
    N = 10, m_arms = 3, outcome_means = c(0, 0, 1))

  design_2[[2]](  design_2[[1]]())
  
  df <- design_2[[1]]()
  po <- declare_potential_outcomes(
    formula = Y ~ 
      (outcome_means[1] + u_1) * (Z == "1") + 
      (outcome_means[2] + u_2) * (Z == "2") + 
      (outcome_means[3] + u_3) * (Z == "3") + u, 
    conditions = c("1", "2", "3"), assignment_variables = Z) 
  po(df)

  outcome_means = 1:3
  outcome_sds = 1:3
  N = 3
  sd_i = 1
  
  po_lib <- design_2[[2]]
  po_new <-  declare_potential_outcomes(
      formula = Y ~ 
        (outcome_means[1] + u_1) * (Z == "1") + 
        (outcome_means[2] + u_2) * (Z == "2") + 
        (outcome_means[3] + u_3) * (Z == "3") + u,
      conditions = c("1",  "2", "3"),
      assignment_variables = Z)

  po_lib(df)
  po_new(df)  
  
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