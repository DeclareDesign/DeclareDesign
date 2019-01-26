test_that("plot works", {
  
  des <-
    declare_population(sleep) +
    declare_estimand(
      CATE_1_5 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 1:5
    ) +
    declare_estimand(
      CATE_6_10 = mean(extra[group == 2]) - mean(extra[group == 1]),
      subset = ID %in% 6:10
    ) +
    declare_step(fabricate, extra = extra + rnorm(N)) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 1:5,
      model = difference_in_means,
      estimand = "CATE_1_5",
      label = "DIM_1_5",
      term = group2
    ) +
    declare_estimator(
      extra ~ group,
      subset = ID %in% 6:10,
      model = difference_in_means,
      estimand = "CATE_6_10",
      label = "DIM_6_10",
      term = group2
    )
  
  # is not right (missing the first estimand and first estimator)
  plot(des)
})