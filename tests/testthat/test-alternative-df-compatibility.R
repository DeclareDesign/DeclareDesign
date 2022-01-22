context("tibble, sf compatibility")

test_that("data.frame", {
  dat <- data.frame(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_model(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})

test_that("tibble", {
  skip_if_not_installed("dplyr")

  dat <- dplyr::tibble(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_model(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})

test_that("tibble more", {
  
  population <- declare_model(N = 100, u = rnorm(N))
  potential_outcomes <- declare_potential_outcomes(Y ~ Z)
  assignment <- declare_assignment(Z = complete_ra(N, m = 50))
  reveal_Y <- declare_reveal(Y,Z)
  
  my_func <- function(data){
    data %>% (tibble::as_tibble)
  }
  
  design <- population + potential_outcomes + assignment + declare_step(handler = my_func)
  
  expect_s3_class(draw_data(design), "tbl_df")
  
})


test_that("data.table", {
  skip_if_not_installed("data.table")

  dat <- data.table::data.table(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_model(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})

test_that("sf", {
  skip_if_not_installed("sf")

  dat <- sf::st_as_sf(data.frame(id = 1:5, y = rnorm(5), x = rnorm(5)), coords = c("x", "y"), remove = FALSE)

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_model(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("id", "y", "x", "geometry", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:6)
})

