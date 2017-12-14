context("tibble, sf compatibility")

test_that("data.frame", {

  dat <- data.frame(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 =  y * 5 + 3)

  design <- declare_design(dat, pos)

  draw_data(design)

})

test_that("tibble", {

  dat <- dplyr::tibble(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 =  y * 5 + 3)

  design <- declare_design(dat, pos)

  draw_data(design)

})


test_that("data.table", {

  dat <- data.table::data.table(y = rnorm(5), x = rnorm(5))

  pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 =  y * 5 + 3)

  design <- declare_design(dat, pos)

  draw_data(design)

})

# test_that("sf", {
#
#   dat <- sf::st_as_sf(data.frame(id = 1:5, y = rnorm(5), x = rnorm(5)), coords = c("x", "y"), remove = FALSE)
#
#   pos <- declare_potential_outcomes(Y_Z_0 = y * 5, Y_Z_1 =  y * 5 + 3)
#
#   design <- declare_design(dat, pos)
#
#   draw_data(design)
#
# })

