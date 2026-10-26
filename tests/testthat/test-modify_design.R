# `insert_step()`, `delete_step()`, and `replace_step()`, plus the index
# resolution they share. `before` comes before `after` in `insert_step()`, as
# it does in 1.x; the rewrite had swapped them, so a positional anchor
# inserted one step later with no message.

test_that("modify_design (insert/delete/replace) still works after deprecation", {
  design <- declare_model(N = 30, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))

  inserted <- suppressWarnings(insert_step(design, declare_measurement(Y2 = Y * 2), after = "model"))
  expect_equal(length(inserted), 3L)

  deleted <- suppressWarnings(delete_step(design, "mu"))
  expect_equal(length(deleted), 1L)

  replaced <- suppressWarnings(replace_step(design, "mu", declare_inquiry(med = median(Y))))
  expect_equal(length(replaced), 2L)
})

test_that("insert_step() keeps 1.x's argument order, before then after", {
  design <- simple_design(N = 20)
  new <- declare_measurement(Y2 = Y * 2)
  positional <- suppressWarnings(insert_step(design, new, "assignment"))
  named <- suppressWarnings(insert_step(design, new, before = "assignment"))
  expect_equal(names(positional), names(named))
  expect_equal(which(names(positional) == "measurement")[1], 3L)
})

test_that("a step locator is a label, an integer, or the step itself", {
  design <- simple_design(N = 20)
  step <- design[["assignment"]]
  by_label <- suppressWarnings(delete_step(design, "assignment"))
  by_index <- suppressWarnings(delete_step(design, 3))
  by_step <- suppressWarnings(delete_step(design, step))
  expect_equal(names(by_label), names(by_index))
  expect_equal(names(by_label), names(by_step))
  expect_false("assignment" %in% names(by_label))
})

test_that("a locator that names no step says so, and so does one of the wrong type", {
  design <- simple_design(N = 20)
  expect_error(suppressWarnings(delete_step(design, "nowhere")),
               "No step named nowhere")
  orphan <- declare_measurement(Y2 = Y * 2, label = "elsewhere")
  expect_error(suppressWarnings(delete_step(design, orphan)),
               "Step labeled elsewhere not found")
  expect_error(suppressWarnings(delete_step(design, list())),
               "must be a label, an integer, or a design_step")
})

test_that("insert_step() needs an anchor, and clamps one that points before the start", {
  design <- simple_design(N = 20)
  new <- declare_measurement(Y2 = Y * 2)
  expect_error(suppressWarnings(insert_step(design, new)),
               "Provide either `after` or `before`")
  first <- suppressWarnings(insert_step(design, new, before = 0))
  expect_equal(names(first)[1], "measurement")
  expect_equal(length(first), length(design) + 1L)
})
