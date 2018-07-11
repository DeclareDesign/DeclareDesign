context("tibble, sf compatibility")

test_that("data.frame", {
  dat <- data.frame(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_population(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})

test_that("tibble", {
  skip_if_not_installed("dplyr")

  dat <- dplyr::tibble(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_population(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})


test_that("data.table", {
  skip_if_not_installed("data.table")

  dat <- data.table::data.table(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_population(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("y", "x", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:4)
})

test_that("sf", {
  skip_if_not_installed("sf")

  dat <- sf::st_as_sf(data.frame(id = 1:5, y = rnorm(5), x = rnorm(5)), coords = c("x", "y"), remove = FALSE)

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 = y * 5 + 3)

  design <- declare_population(dat) + pos

  df <- draw_data(design)

  expect_identical(colnames(df), c("id", "y", "x", "geometry", "Y_Z_0", "Y_Z_1"))

  expect_identical(dim(df), 5:6)
})
