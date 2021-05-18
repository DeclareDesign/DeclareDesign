context("Inquiries")

df <- data.frame(Y_Z_0 = 1:10, Y_Z_1 = 3:12)

test_that("splat labels", {
  ## default labeling
  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  expect_identical(
    my_inquiry(df),
    structure(list(inquiry = "ATE", estimand = 2), .Names = c(
      "inquiry",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_inquiry, "label"), "ATE")
})

test_that("default label", {
  ## no label
  my_inquiry <- declare_inquiry(mean(Y_Z_1 - Y_Z_0))
  expect_identical(
    my_inquiry(df),
    structure(list(inquiry = "inquiry", estimand = 2), .Names = c(
      "inquiry",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_inquiry, "label"), "inquiry")
})

test_that("manual label", {

  ## manual label
  my_inquiry <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "ATE2")

  expect_identical(
    my_inquiry(df),
    structure(list(inquiry = "ATE2", estimand = 2), .Names = c(
      "inquiry",
      "estimand"
    ), row.names = c(NA, -1L), class = "data.frame")
  )
  expect_equal(attr(my_inquiry, "label"), "ATE2")
})

test_that("custom inquiry has label", {
  ## custom inquiry function
  my_inquiry_function <- function(data, label) {
    with(data, data.frame(inquiry = label, estimand = median(Y_Z_1 - Y_Z_0), stringsAsFactors = TRUE))
  }
  my_inquiry_custom <- declare_inquiry(
    handler = my_inquiry_function, label = "medianTE"
  )

  expect_identical(
    my_inquiry_custom(df),
    structure(list(
      inquiry = structure(1L, .Label = "medianTE", class = "factor"),
      estimand = 2
    ), .Names = c("inquiry", "estimand"), row.names = c(
      NA,
      -1L
    ), class = "data.frame")
  )
  expect_equal(attr(my_inquiry_custom, "label"), "medianTE")
})

test_that("splat label overrides label", {
  my_inquiry <- declare_inquiry(SATT = mean(Y_Z_1 - Y_Z_0), label = "ATE")
  expect_equal(
    attributes(my_inquiry)$label,
    "SATT"
  )
})


test_that("multiple inquiry declarations work", {
  # splat label, should inherit
  sate <- declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0))
  pate <- declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0))

  design_1 <- declare_population(df) + pate + sate
  expect_identical(
    draw_estimands(design_1),
    structure(list(inquiry = c("PATE", "SATE"), estimand = c(
      2,
      2
    )), .Names = c("inquiry", "estimand"), row.names = c(
      NA,
      -2L
    ), class = "data.frame")
  )
})

test_that("multiple inquiry declarations work", {

  # Explicit label, should not inherit
  sate_label <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "The SATE")
  pate_label <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "The PATE")

  design_2 <- declare_population(df) + pate_label + sate_label

  expect_identical(
    draw_estimands(design_2),
    structure(list(inquiry = c("The PATE", "The SATE"), estimand = c(
      2,
      2
    )), .Names = c("inquiry", "estimand"), row.names = c(
      NA,
      -2L
    ), class = "data.frame")
  )
})

test_that("duplicated labels fail", {
  # This could eventually be fixed so that the inquiry object names are inherited
  # default labeling whatsoever
  sate_nolabel <- declare_inquiry(mean(Y_Z_1 - Y_Z_0))
  pate_nolabel <- declare_inquiry(mean(Y_Z_1 - Y_Z_0))

  expect_error({
    design_3 <- declare_population(df) + pate_nolabel + sate_nolabel
  })
})


test_that("inquiries can use other inquiries in calculations", {
  prop_inquiry <- declare_inquiry(yz1.mu = mean(Y_Z_1), yz0.mu = mean(Y_Z_0), percent.diff = abs(yz1.mu - yz0.mu) / yz0.mu)

  expect_equal(
    prop_inquiry(df),
    structure(list(inquiry = c("yz1.mu", "yz0.mu", "percent.diff"), 
                   estimand = c(7.5, 5.5, 0.363636363636364)), 
              class = "data.frame", row.names = c(NA, -3L))
  )
})
