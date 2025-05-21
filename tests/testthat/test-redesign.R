context("redesign")

test_that("N not changed", {
  N <- 100
  design <- declare_model(N = N) + NULL
  expect_equal(N, 100)
  
  expect_true(DeclareDesign:::find_all_objects(design)$value_str == 100)
  expect_length(draw_data(design)$ID, 100)
  
  design2 <- redesign(design, N = 20)
  expect_length(draw_data(design2)$ID, 20)

  DeclareDesign:::find_all_objects(design2)
  draw_data(design2) |> nrow()
  
  others <- c(50, 100, 200, 99)
  d_alt <- redesign(design, N = others)

  for (i in seq_along(others)) {
    expect_length(draw_data(d_alt[[i]])$ID, others[i])
  }

  # N itself should not be changed
  expect_equal(N, 100)
})
