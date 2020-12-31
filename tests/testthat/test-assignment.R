context("Assignment and probability functions")

test_that("use of randomizr works", {
  
  design <- declare_model(
    classrooms = add_level(10),
    individuals = add_level(20, female = rbinom(N, 1, 0.5))
  ) + NULL
  
  dat <- draw_data(design)
  
  assgn1 <- declare_assignment(Z = complete_ra(N = N, m = 10))
  
  expect_equal(sum(assgn1(dat)$Z), 10)
  
})


