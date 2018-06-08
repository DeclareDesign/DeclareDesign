

context("functions in templates")

test_that("simple template works", {
my_template <- function(N = 100,
                        my_estimand_func = mean) {
  my_pop <- declare_population(N = N, Y = rnorm(N))
  my_estimand <- declare_estimand(mand = my_estimand_func(Y))
  my_design <- my_pop + my_estimand
  my_design
}

expect_length(design_list <-
  expand_design(
    template = my_template,
    N = c(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
    my_estimand_func = c(mean, median)
  ), 18)

expect_length(design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = c(mean, median)
  ), 6)

  expect_length(design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = list(mean, median)
  ), 6)


  expect_length(design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = mean
  ), 3)
})


test_that("template with vector argument works", {
  my_template <- function(N = c(100, 50),
                          my_estimand_func = mean) {
    my_pop <- declare_population(N = min(N), Y = rnorm(N))
    my_estimand <- declare_estimand(mand = my_estimand_func(Y))
    my_design <- my_pop + my_estimand
    my_design
  }
  
  expect_length(design_list <-
    expand_design(
      template = my_template,
      N = list(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
      my_estimand_func = c(mean, median)
    ), 6)
})




context("functions in templates")

my_template <- function(N = 100,
                        my_estimand_func = mean) {
  my_pop <- declare_population(N = N, Y = rnorm(N))
  my_estimand <- declare_estimand(mand = my_estimand_func(Y))
  my_design <- my_pop + my_estimand
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

