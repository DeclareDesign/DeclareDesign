context("Assignment and probability functions")

test_that("randomizr works through declare_assignment", {
  df <- data.frame(ID = 1:10, block_var = rep(c("A", "B"), 5, 5))

  f_1 <- declare_assignment()
  f_1(df)

  debugonce(declare_assignment)
  f_1 <- declare_assignment(m = 5)
  f_1(df)

  f_1 <- declare_assignment(num_arms = 2)
  f_1(df)

  f_1 <- declare_assignment(num_arms = 3)
  f_1(df)

  f_1 <- declare_assignment(block_var = block_var)
  f_1(df)


  # what about inside a function?

  new_fun <- function(num_arms){
    f_1 <- declare_assignment(num_arms = num_arms)
    f_1(df)
  }
  new_fun(3)

})




test_that("test assignment and probability functions", {

  library(magrittr)
  population <- declare_population(
    villages = level(N = 100, elevation = rnorm(N),
                     high_elevation = as.numeric(elevation > 0)),
    individuals = level(N = 10, noise = rnorm(N),
                        ideo_3 = sample(c('Liberal', 'Moderate', 'Conservative'),
                                        size = N, prob = c(.2, .3, .5), replace = TRUE))
  )

  sampling <- declare_sampling(n = 10, clust_var = villages_ID)

  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1, 2),
                                                   assignment_variable_name = "Z")


#  population() %>% sampling() %>% potential_outcomes()


  # Complete Random Assignment assignments
  assignment_0 <- declare_assignment() # blug
  assignment_1 <- declare_assignment(condition_names = c(0, 1))
  assignment_2 <- declare_assignment(m = 60, condition_names = c(0, 1))
  assignment_3 <- declare_assignment(m_each = c(20, 30, 50))
  assignment_4 <- declare_assignment(m_each =c(20, 80), condition_names = c(0, 1))
  assignment_5 <- declare_assignment(prob_each = c(.2, .3, .5))

  # Blocked assignments
  assignment_6 <- declare_assignment(block_var = ideo_3)
  assignment_7 <- declare_assignment(block_var = ideo_3, prob_each = c(.3, .6, .1))
  assignment_8 <- declare_assignment(block_var = ideo_3, condition_names = c(0, 1))

  assignment_9 <- declare_assignment(block_var = ideo_3,
                                       condition_names = c(0, 1),
                                       block_m = c(10, 10, 10))


  # Clustered assignments
  assignment_10 <- declare_assignment(clust_var = villages_ID)
  assignment_11 <- declare_assignment(clust_var = villages_ID, condition_names = c(0, 1))
  assignment_12 <- declare_assignment(clust_var = villages_ID, prob_each = c(.1, .3, .6))

  # Blocked and Clustered assignments
  assignment_13 <- declare_assignment(clust_var = villages_ID,
                                      block_var = high_elevation)

  assignment_14 <- declare_assignment(clust_var = villages_ID,
                                      block_var = high_elevation, condition_names = c(0,1))
  assignment_15 <- declare_assignment(clust_var = villages_ID,
                                      block_var = high_elevation, prob_each = c(.1, .3, .6))

  # Draw Data
  smp_draw <- population() %>% sampling() %>% potential_outcomes()

  smp_draw %>% head
  # Attempt to Assign

  smp_draw %>% assignment_0() %$% table(Z)
  smp_draw %>% assignment_1() %$% table(Z)
  smp_draw %>% assignment_2() %$% table(Z)
  smp_draw %>% assignment_3() %$% table(Z)
  smp_draw %>% assignment_4() %$% table(Z)
  smp_draw %>% assignment_5() %$% table(Z)
  smp_draw %>% assignment_6() %$% table(ideo_3, Z)
  smp_draw %>% assignment_7() %$% table(ideo_3, Z)
  smp_draw %>% assignment_8() %$% table(ideo_3, Z)
  smp_draw %>% assignment_9() %$% table(ideo_3, Z)
  smp_draw %>% assignment_10() %$% table(villages_ID, Z)
  smp_draw %>% assignment_11() %$% table(villages_ID, Z)
  smp_draw %>% assignment_12() %$% table(villages_ID, Z)
  smp_draw %>% assignment_13() %$% table(villages_ID, Z)
  smp_draw %>% assignment_14() %$% table(villages_ID, Z)
  smp_draw %>% assignment_15() %$% table(villages_ID, Z)

  # Obtain Treatment Probabilities
  smp_draw %>% assignment_0() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_1() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_2() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_3() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_4() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_5() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_6() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_7() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_8() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_9() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_10() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_11() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_12() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_13() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_14() %$% Z_cond_prob %>% head()
  smp_draw %>% assignment_15() %$% Z_cond_prob %>% head()

})


