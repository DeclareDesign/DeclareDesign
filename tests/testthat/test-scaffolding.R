context("Internal declaration scaffolding")

test_that("make declarations correctly",{

  noop_handler <- function(data) data
  i <<- 0
  validation_fn(noop_handler) <- function(ret, dots, label) { i <<- label; ret }

  declare_noop <- make_declarations(noop_handler, "test", causal_type='dgp', "noop", strictDataParam=TRUE)

  expect_equal(formals(declare_noop)$handler, quote(noop_handler))


  expect_equal(class(declare_noop), c("declaration", "function"))

  expect_equal(names(attributes(declare_noop)), c("class", "step_type", "causal_type", "strictDataParam"))

  expect_equal(i, 0) # i is unchanged so far
  my_noop <- declare_noop(handler=noop_handler, label="booyah") #setting handler here? scoping issue from setting default handler due to package protection
  expect_equal(i, "booyah") # i is set via the validation callback

  expect_identical(sleep, my_noop(sleep))
})
