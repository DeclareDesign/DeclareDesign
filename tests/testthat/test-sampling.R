
test_that("randomizr works through declare_sampling", {
  df <- data.frame(ID = 1:10, strata_var = rep(c("A", "B"), 5, 5))

  f_1 <- declare_sampling()
  f_1(df)

  f_1 <- declare_sampling(n = 5)
  f_1(df)

  f_1 <- declare_sampling(strata_var = strata_var)
  f_1(df)


  # what about inside a function?

  new_fun <- function(n){
    f_1 <- declare_sampling(n = n)
    f_1(df)
  }
  new_fun(3)

})



context("Sampling and probability functions")

test_that("test sampling and probability functions", {

  library(magrittr)
  population <- declare_population(
    villages = level(N = 100, elevation = rnorm(N),
                     high_elevation = as.numeric(elevation > 0)),
    individuals = level(N = 10, noise = rnorm(N),
                        ideo_3 = sample(c('Liberal', 'Moderate', 'Conservative'),
                                        size = N, prob = c(.2, .3, .5), replace = TRUE))
)
  # "complete" sampling
  sampling_1 <- declare_sampling()
  sampling_2 <- declare_sampling(n = 60)

  # stratified sampling
  sampling_3 <- declare_sampling(strata_var = ideo_3)
  sampling_4 <- declare_sampling(strata_var = ideo_3, strata_prob = c(.3, .6, .1))

  sampling_5 <- declare_sampling(strata_var = ideo_3,
                                 strata_n = c(10, 10, 10))

  # Clustered sampling
  sampling_6 <- declare_sampling(clust_var = villages_ID)

  # Stratified and Clustered assignments
  sampling_7 <- declare_sampling(clust_var = villages_ID,
                                      strata_var = high_elevation)

  # Draw Data
  smp_draw <- population()

  # Attempt to Assign

  smp_draw %>% nrow
  smp_draw %>% sampling_1() %>% nrow
  smp_draw %>% sampling_2() %>% nrow
  smp_draw %>% sampling_3() %$% table(ideo_3)
  smp_draw %>% sampling_4() %$% table(ideo_3)
  smp_draw %>% sampling_5() %$% table(ideo_3)
  smp_draw %>% sampling_6() %$% table(villages_ID)
  smp_draw %>% sampling_7() %$% table(villages_ID, high_elevation)

  # Obtain Treatment Probabilities
  smp_draw %>% sampling_1() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_2() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_3() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_4() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_5() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_6() %$% S_inclusion_prob %>% head()
  smp_draw %>% sampling_7() %$% S_inclusion_prob %>% head()

})

