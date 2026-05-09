test_that("diagnose_design accepts custom diagnosands via set_diagnosands", {
  design <- simple_design(N = 30) |>
    set_diagnosands(declare_diagnosands(
      mean_estimate = mean(estimate, na.rm = TRUE)
    ))
  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_true("mean_estimate" %in% names(get_diagnosands(d)))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("select_diagnosands subsets diagnosands", {
  diags <- default_diagnosands()
  trimmed <- select_diagnosands(diags, "bias", "rmse")
  expect_equal(names(attr(trimmed, "dots")), c("bias", "rmse"))
})

test_that("diagnose_design over multiple designs adds a design column", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  d <- diagnose_design(!!!fam, sims = 5, bootstrap_sims = 0)
  diag <- get_diagnosands(d)
  expect_true("design" %in% names(diag))
  expect_equal(nrow(diag), 2L)
})

test_that("modify_design (insert/delete/replace) works", {
  design <- declare_model(N = 30, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))

  inserted <- insert_step(design,
    declare_measurement(Y2 = Y * 2),
    after = "model")
  expect_equal(length(inserted), 3L)

  deleted <- delete_step(design, "mu")
  expect_equal(length(deleted), 1L)

  replaced <- replace_step(design, "mu",
    declare_inquiry(med = median(Y)))
  expect_equal(length(replaced), 2L)
})
