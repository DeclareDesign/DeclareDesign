context("Estimands")

df <- data.frame(Y_Z_0 = 1:10, Y_Z_1 = 3:12)

test_that("splat labels", {
  ## default labeling
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  expect_identical(
    my_estimand(df),
    structure(list(estimand_label = "ATE", estimand = 2), .Names = c(
      "estimand_label",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_estimand, "label"), "ATE")
})

test_that("default label", {
  ## no label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  expect_identical(
    my_estimand(df),
    structure(list(estimand_label = "estimand", estimand = 2), .Names = c(
      "estimand_label",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_estimand, "label"), "estimand")
})

test_that("manual label", {

  ## manual label
  my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "ATE2")

  expect_identical(
    my_estimand(df),
    structure(list(estimand_label = "ATE2", estimand = 2), .Names = c(
      "estimand_label",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_estimand, "label"), "ATE2")
})

test_that("custom estimand has label", {
  ## custom estimand function
  my_estimand_function <- function(data, label) {
    with(data, data.frame(estimand_label = label, estimand = median(Y_Z_1 - Y_Z_0)))
  }
  my_estimand_custom <- declare_estimand(
    handler = my_estimand_function, label = "medianTE"
  )

  expect_identical(
    my_estimand_custom(df),
    structure(list(
      estimand_label = structure(1L, .Label = "medianTE", class = "factor"),
      estimand = 2
    ), .Names = c("estimand_label", "estimand"), row.names = c(
      NA,
      -1L
    ), class = "data.frame")
  )
  expect_equal(attr(my_estimand_custom, "label"), "medianTE")
})

test_that("splat label overrides label", {
  my_estimand <- declare_estimand(SATT = mean(Y_Z_1 - Y_Z_0), label = "ATE")
  expect_equal(
    attributes(my_estimand)$label,
    "SATT"
  )
})


test_that("multiple estimand declarations work", {
  # splat label, should inherit
  sate <- declare_estimand(SATE = mean(Y_Z_1 - Y_Z_0))
  pate <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))

  design_1 <- declare_population(df) + pate + sate
  expect_identical(
    draw_estimands(design_1),
    structure(list(estimand_label = c("PATE", "SATE"), estimand = c(
      2,
      2
    )), .Names = c("estimand_label", "estimand"), row.names = c(
      NA,
      -2L
    ), class = "data.frame")
  )
})

test_that("multiple estimand declarations work", {

  # Explicit label, should not inherit
  sate_label <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "The SATE")
  pate_label <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "The PATE")

  design_2 <- declare_population(df) + pate_label + sate_label

  expect_identical(
    draw_estimands(design_2),
    structure(list(estimand_label = c("The PATE", "The SATE"), estimand = c(
      2,
      2
    )), .Names = c("estimand_label", "estimand"), row.names = c(
      NA,
      -2L
    ), class = "data.frame")
  )
})

test_that("duplicated labels fail", {
  # This could eventually be fixed so that the estimand object names are inherited
  # default labeling whatsoever
  sate_nolabel <- declare_estimand(mean(Y_Z_1 - Y_Z_0))
  pate_nolabel <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

  expect_error({
    design_3 <- declare_population(df) + pate_nolabel + sate_nolabel
  })
})
