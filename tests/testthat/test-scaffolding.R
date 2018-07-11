context("Internal declaration scaffolding")

test_that("make declarations correctly", {
  noop_handler <- function(data) data
  i <<- 0
  validation_fn(noop_handler) <- function(ret, dots, label) {
    i <<- label
    ret
  }

  declare_noop <- make_declarations(noop_handler, "test", causal_type = "dgp", "noop", strictDataParam = TRUE)

  expect_equal(formals(declare_noop)$handler, quote(noop_handler))


  expect_equal(class(declare_noop), c("declaration", "function"))

  expect_equal(names(attributes(declare_noop)), c("class", "step_type", "causal_type", "strictDataParam"))

  expect_equal(i, 0) # i is unchanged so far
  my_noop <- declare_noop(handler = noop_handler, label = "my_label") # setting handler here? scoping issue from setting default handler due to package protection
  expect_equal(i, "my_label") # i is set via the validation callback

  expect_identical(sleep, my_noop(sleep))
})

test_that("internal testing function was built correctly", {

  # declare_internal_inherit_params - step template
  expect_true(inherits(declare_internal_inherit_params, "declaration"))
  expect_true(is.function(declare_internal_inherit_params))
  expect_equal(attr(declare_internal_inherit_params, "step_type"), "BLNKMSG")
  expect_equal(attr(declare_internal_inherit_params, "causal_type"), "dgp")
  expect_true(attr(declare_internal_inherit_params, "strictDataParam"))

  w <- declare_internal_inherit_params(foo)
  expect_true(inherits(w, "design_step"))
  expect_true(is.function(w))
  expect_equal(attr(w, "step_type"), "BLNKMSG")
  expect_equal(attr(w, "causal_type"), "dgp")
  expect_identical(as.character(attr(w, "call")), as.character(quote(declare_internal_inherit_params(foo))))

  expect_identical(w(NULL), structure(list(HIA = structure(1L, .Label = "MSG", class = "factor")),
    .Names = "BLNK", row.names = c(NA, -1L), class = "data.frame"
  ))
})
