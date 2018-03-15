context("testing rlang NSE syntax sugar")


test_that("multiple PO / reveal", {

  my_population <- declare_population(N = 100, noise = rnorm(N))

  multi_po <- rlang:::quos()
  multi_po[paste0("Y", 1:3, "_Z_0")] <- list(rlang::quo(noise))
  multi_po[paste0("Y", 1:3, "_Z_1")] <- list(rlang::quo(noise + rnorm(N, mean=2, sd=2)))


  my_potential_outcomes3 <- declare_potential_outcomes(!!!multi_po)


  my_assignment <- declare_assignment(m = 50)

  my_outcomes <- paste0('Y', 1:3)

  reveal_multiple <- declare_reveal(outcome_variables = !!my_outcomes)


  design <- declare_design(
    my_population,
    my_potential_outcomes3,
    my_assignment,
    reveal_multiple
  )

  expect_equal(grep("^Y\\d$", colnames(draw_data(design)), value = TRUE), my_outcomes)


})


test_that("slash constructors", {
  d <- declare_population(sleep) / declare_sampling() / declare_assignment()
  expect_equal(dim(draw_data(d)), c(10,6))
  expect_equal(deparse(attr(d, "call")), "declare_population(sleep)/declare_sampling()/declare_assignment()")
})
