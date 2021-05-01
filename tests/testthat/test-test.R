
context("declare_test")

test_that("declare_test works", {
  skip_if_not_installed("coin")
  
  # thanks to Jake Bowers for this test
  # https://gist.github.com/jwbowers/2d91fd40faa13520b29ebaa187c58e17
  
  our_ttest <- function(data) {
    require(coin)
    res <- coin::oneway_test(
      disp ~ factor(vs),
      data = data,
      distribution = "asymptotic"
    )
    data.frame(p.value = pvalue(res)[[1]])
  }
  
  des <- 
    # declare_population(N = 100, Xclus = rbinom(n = N, size = 1, prob = 0.2), outcome = 0.2 * Xclus + 3 + rnorm(N)) +
    declare_population(data = mtcars) + 
    declare_test(handler = our_ttest, label = "bare") + 
    declare_test(handler = label_test(our_ttest), label = "tidied")
  
  est <- draw_estimates(des)
  
  expect_equal(est, structure(list(p.value = c(7.6397665500938e-05, 7.6397665500938e-05
  ), estimator = c(NA, "tidied")), row.names = c(NA, -2L), class = "data.frame"))
  
})