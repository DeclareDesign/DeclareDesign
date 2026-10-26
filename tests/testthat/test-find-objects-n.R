# Which `N` a step reads: the rows in hand, or the workspace ----

test_that("a workspace N is not reported where N counts rows", {
  pop <- fabricatr::fabricate(N = 30, Y = rnorm(N))
  N <- 7
  on_data <- declare_model(data = pop) +
    declare_sampling(S = complete_rs(N, n = 5)) + NULL
  expect_false("N" %in% find_all_objects(on_data)$name)

  on_levels <- declare_model(v = add_level(N = 4, u = rnorm(N))) +
    declare_sampling(S = complete_rs(N, n = 2)) +
    declare_measurement(M = N) + NULL
  expect_false("N" %in% find_all_objects(on_levels)$name)
  expect_equal(nrow(draw_data(on_levels)), 2)
})

test_that("the value of an argument named N reads the workspace", {
  N <- 7
  top <- declare_model(N = N, U = rnorm(N)) + NULL
  expect_equal(find_all_objects(top)$step[find_all_objects(top)$name == "N"], 1)

  nested <- declare_model(N = 5, U = rnorm(N)) +
    declare_model(h = nest_level(N = N, k = 1)) + NULL
  objs <- find_all_objects(nested)
  expect_equal(objs$step[objs$name == "N"], 2)
  expect_equal(nrow(draw_data(nested)), 35)
  expect_equal(nrow(draw_data(redesign(nested, N = 2))), 10)
})
