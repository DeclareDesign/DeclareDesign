context("Utilities")



test_that("pretty printers", {
  expect_output(print(declare_population(sleep)),
                "declare_population\\(sleep\\)")
})

test_that("error if data is in there.", {
  expect_error(declare_potential_outcomes(data = "foo"),
               "should not be a declared argument.")
})

test_that("fallback to lapply", {
  future_lapply <- future_lapply
  environment(future_lapply) <-
    new.env(parent = environment(future_lapply))
  environment(future_lapply)$requireNamespace <- function(...)
    FALSE
  
  expect_identical(future_lapply(LETTERS, identity), as.list(LETTERS))
})


test_that("names from quos", {
  blank_fun <- function(select) {
    reveal_nse_helper(enquo(select))
  }
  
  expect_equal("bias", blank_fun(select = bias))
  expect_equal(c("bias", "mean"), blank_fun(select = c(bias, mean)))
})


test_that("clone_dot_edit_env", {
  dot <- quo(test_obj)
  environment(dot) <- NULL
  expect_s3_class(clone_dot_edit_env(dot,
                                     here_i_am = "some_message", 
                                     xyxyx = "bar"),
                  "quosure")
})
