

context("functions in templates")

test_that("simple template works", {
my_template <- function(N = 100,
                        my_estimand_func = mean) {
  my_pop <- declare_population(N = N, Y = rnorm(N))
  my_estimand <- declare_estimand(mand = my_estimand_func(Y))
  my_design <- my_pop + my_estimand
  my_design
}

# debugonce(DeclareDesign:::expand_design)
design_list <-
  expand_design(
    template = my_template,
    N = c(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
    my_estimand_func = c(mean, median)
  )

design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = c(mean, median)
  )

design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = list(mean, median)
  )


design_list <-
  expand_design(
    template = my_template,
    N = c(10, 50, 100),
    my_estimand_func = mean
  )
})


test_that("template with vector argument works", {
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
    ), 6)
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = c(mean, median)
    )
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = list(mean, median)
    )
  
  
  design_list <-
    expand_design(
      template = my_template,
      N = c(10, 50, 100),
      my_estimand_func = mean
    )
})


