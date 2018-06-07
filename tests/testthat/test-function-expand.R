

context("functions in templates")

my_template <- function(N = 100,
                        my_estimand_func = mean) {
  my_pop <- declare_population(N = N, Y = rnorm(N))
  my_estimand <- declare_estimand(mand = my_estimand_func(Y))
  my_design <- declare_design(my_pop, my_estimand)
  my_design
}


test_that("expand_design works",{
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
      my_estimand_func = c(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_estimand_func") %in% names(diag$diagnosands_df)))
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = c(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_estimand_func") %in% names(diag$diagnosands_df)))
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = list(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_estimand_func") %in% names(diag$diagnosands_df)))
  
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = mean
    )
  
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_estimand_func") %in% names(diag$diagnosands_df)))
  
})


library(DesignLibrary)

designs1 = expand_design(simple_two_arm_designer, rho = c(.3, .4), prefix = "rho_design")

designs2 = expand_design(simple_two_arm_designer, N = 6:7, ate = 1:2, prefix = "mo_design")

designs3 <- c(designs1, designs2)

sims1 <- simulate_design(designs1, sims = 2)
sims2 <- simulate_design(designs2, sims = 2)
sims3 <- simulate_design(designs3, sims = 2)


diag1 <- diagnose_design(designs1, sims = 2, bootstrap_sims = FALSE)
str(diag1)
diag2 <- diagnose_design(designs2, sims = 2, bootstrap_sims = FALSE)
str(diag2)
diag3 <- diagnose_design(designs3, sims = 2, bootstrap_sims = FALSE)
str(diag3)