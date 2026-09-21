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
