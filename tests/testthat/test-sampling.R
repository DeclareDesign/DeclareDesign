context("Sampling and probability functions")

test_that("randomizr works through declare_sampling", {
  df <- data.frame(ID = 1:10, strata = rep(c("A", "B"), 5, 5))

  f_1 <- declare_sampling()
  expect_equal(dim(f_1(df)), c(5,3))

  f_1 <- declare_sampling(n = 4)
  expect_equal(dim(f_1(df)), c(4,3))

  f_1 <- declare_sampling(strata = strata)
  expect_length(xtabs(~strata, f_1(df)), 2)


  # what about inside a function?

  new_fun <- function(n){
    f_1 <- declare_sampling(n = n)
    f_1(df)
  }
  expect_equal(dim(new_fun(3)), c(3,3))

})



test_that("test sampling and probability functions", {

  population <- declare_population(
    villages = add_level(N = 100, elevation = rnorm(N),
                     high_elevation = as.numeric(elevation > 0)),
    individuals = add_level(N = 10, noise = rnorm(N),
                        ideo_3 = sample(c('Liberal', 'Moderate', 'Conservative'),
                                        size = N, prob = c(.2, .3, .5), replace = TRUE))
)
  # "complete" sampling
  sampling_1 <- declare_sampling()
  sampling_2 <- declare_sampling(n = 60)

  # stratified sampling
  sampling_3 <- declare_sampling(strata = ideo_3)
  sampling_4 <- declare_sampling(strata = ideo_3, strata_prob = c(.3, .6, .1))

  sampling_5 <- declare_sampling(strata = ideo_3,
                                 strata_n = c(10, 10, 10))

  # Clustered sampling
  sampling_6 <- declare_sampling(clusters = villages)

  # Stratified and Clustered assignments
  sampling_7 <- declare_sampling(clusters = villages,
                                      strata = high_elevation)

  # Draw Data
  smp_draw <- population()

  # Attempt to Assign

  smp_draw %>% nrow
  smp_draw %>% sampling_1() %>% nrow
  smp_draw %>% sampling_2() %>% nrow
  smp_draw %>% sampling_3() %>% with(.,table(ideo_3))
  smp_draw %>% sampling_4() %>% with(.,table(ideo_3))
  smp_draw %>% sampling_5() %>% with(.,table(ideo_3))
  smp_draw %>% sampling_6() %>% with(.,table(villages))
  smp_draw %>% sampling_7() %>% with(.,table(villages, high_elevation))

  # Obtain Treatment Probabilities
  smp_draw %>% sampling_1() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_2() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_3() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_4() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_5() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_6() %>% .$S_inclusion_prob %>% head()
  smp_draw %>% sampling_7() %>% .$S_inclusion_prob %>% head()

})


test_that("declare_assignment expected failures via validation fn", {

  expect_true(is.function(declare_sampling()))

  expect_error(declare_sampling(strata='character'), "strata")

  expect_error(declare_sampling(clusters='character'), "clusters")

  expect_error(declare_sampling(sampling_variable_name = NULL), "sampling_variable_name")
})



