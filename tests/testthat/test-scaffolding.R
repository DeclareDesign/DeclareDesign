context("Internal declaration scaffolding")

test_that("make declarations correctly",{

  declare_noop <- make_declarations(function(data) data, "test", causal_type='dgp', "noop", validation=I, strictDataParam=TRUE)

  expect_equal(class(declare_noop), c("declaration", "function"))

  expect_equal(names(attributes(declare_noop)), c("class", "step_type", "causal_type", "strictDataParam"))

  my_noop <- declare_noop(label="no")

  expect_identical(sleep, my_noop(sleep))
})
