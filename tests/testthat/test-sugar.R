context("testing rlang NSE syntax sugar")


test_that("multiple PO / reveal", {
  my_population <- declare_population(N = 100, noise = rnorm(N))

  multi_po <- rlang:::quos()
  multi_po[paste0("Y", 1:3, "_Z_0")] <- list(rlang::quo(noise))
  multi_po[paste0("Y", 1:3, "_Z_1")] <- list(rlang::quo(noise + rnorm(N, mean = 2, sd = 2)))

  my_potential_outcomes3 <- declare_potential_outcomes(!!!multi_po)

  my_assignment <- declare_assignment(m = 50)

  my_outcomes <- paste0("Y", 1:3)

  reveal_multiple <- reveal_outcomes(outcome_variables = !!my_outcomes)

  design <- my_population + my_potential_outcomes3 + my_assignment + reveal_multiple

  expect_equal(grep("^Y\\d$", colnames(draw_data(design)), value = TRUE), my_outcomes)
})


test_that("+ constructors", {
  d <- declare_population(sleep) + declare_sampling() + declare_assignment()
  expect_equal(dim(draw_data(d)), c(10, 6))
})



test_that("Lots of levels", {
  outcomes <- lapply(LETTERS, function(l) quo(preference == !!l))
  names(outcomes) <- paste0("Y_Z_", LETTERS)


  design <- declare_population(N = 26000, preference = sample(LETTERS, N, replace = TRUE)) +
    declare_potential_outcomes(!!!outcomes) +
    declare_assignment(conditions = !!LETTERS) +
    reveal_outcomes()

  expect_equal(colnames(draw_data(design)), c(
    "ID", "preference", "Y_Z_A", "Y_Z_B", "Y_Z_C", "Y_Z_D", "Y_Z_E",
    "Y_Z_F", "Y_Z_G", "Y_Z_H", "Y_Z_I", "Y_Z_J", "Y_Z_K", "Y_Z_L",
    "Y_Z_M", "Y_Z_N", "Y_Z_O", "Y_Z_P", "Y_Z_Q", "Y_Z_R", "Y_Z_S",
    "Y_Z_T", "Y_Z_U", "Y_Z_V", "Y_Z_W", "Y_Z_X", "Y_Z_Y", "Y_Z_Z",
    "Z", "Z_cond_prob", "Y"
  ))
})
