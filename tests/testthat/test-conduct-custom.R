context("declare design")

test_that("test the custom execution strategy", {

  # closes ticket #62

  design <- declare_population(sleep) + declare_estimator(extra ~ group)

  my_sleep <- sleep
  my_sleep$extra <- my_sleep$extra + 1 * (my_sleep$group == 1)

  exst <-
    execution_st(design,
      current_df = my_sleep,
      results = list(estimator = vector(mode = "list", length = 2)),
      2, 2
    )


  regular <- run_design(design)
  output <- run_design(exst)

  expect_equal(
    regular$estimates_df$estimate,
    output$estimates_df$estimate + 1
  )
  expect_equal(names(output), "estimates_df") # no estimands
})


test_that("test error messages in run_design", {

  # closes ticket #12
  design <- declare_population(sleep) + declare_population(foo = bar)

  expect_error(run_design(design), "Error in step 2")
})

test_that("draw_data does not run estimand/estimator", {

  # closes ticket #12
  design <- declare_population(sleep) +
    declare_estimand(
      "Should not be run",
      handler = function(data, msg)
        stop(x)
    )

  expect_identical(draw_data(design), sleep)
})
