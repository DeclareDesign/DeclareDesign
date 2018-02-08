context("Assignment and probability functions")

test_that("randomizr works through declare_assignment", {
  df <- data.frame(ID = 1:10, blocks = rep(c("A", "B"), 5, 5))

  f_1 <- declare_assignment()
  expect_equal( sum(f_1(df)$Z), 5)

  f_1 <- declare_assignment(m = 5)
  expect_equal( sum(f_1(df)$Z), 5)

  f_1 <- declare_assignment(num_arms = 2)
  expect_equal( sum(f_1(df)$Z == "T1"), 5)

  f_1 <- declare_assignment(num_arms = 3)
  expect_true( all( table(f_1(df)$Z) >= 3) )

  f_1 <- declare_assignment(blocks = blocks)
  expect_true(all.equal(
    unclass(xtabs(~blocks+Z, f_1(df))),
    matrix(c(3,2,3,2),2,2), #slight bug in the blocks above with rep(AB,5,5) => ABABA x 2
    check.attributes=FALSE
  ))


  # what about inside a function?

  new_fun <- function(num_arms){
    f_1 <- declare_assignment(num_arms = num_arms)
    f_1(df)
  }
  new_fun(3)

})




test_that("test assignment and probability functions", {

  population <- declare_population(
    villages = add_level(N = 100, elevation = rnorm(N),
                     high_elevation = as.numeric(elevation > 0)),
    individuals = add_level(N = 10, noise = rnorm(N),
                        ideo_3 = sample(c('Liberal', 'Moderate', 'Conservative'),
                                        size = N, prob = c(.2, .3, .5), replace = TRUE))
  )

  sampling <- declare_sampling(n = 10, clusters = villages)

  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1, 2),
                                                   assignment_variable_name = "Z")


#  population() %>% sampling() %>% potential_outcomes()


  # Complete Random Assignment assignments
  assignment_0 <- declare_assignment() # blug
  assignment_1 <- declare_assignment(condition_names = c(0, 1))
  assignment_2 <- declare_assignment(m = 60, condition_names = c(0, 1))
  assignment_3 <- declare_assignment(m_each = c(20, 30, 50), reveal = FALSE)
  assignment_4 <- declare_assignment(m_each =c(20, 80), condition_names = c(0, 1))
  assignment_5 <- declare_assignment(prob_each = c(.2, .3, .5), reveal=FALSE)

  # Blocked assignments
  assignment_6 <- declare_assignment(blocks = ideo_3)
  assignment_7 <- declare_assignment(blocks = ideo_3, prob_each = c(.3, .6, .1), reveal=FALSE)
  assignment_8 <- declare_assignment(blocks = ideo_3, condition_names = c(0, 1))

  assignment_9 <- declare_assignment(blocks = ideo_3,
                                       condition_names = c(0, 1),
                                       block_m = c(10, 10, 10))


  # Clustered assignments
  assignment_10 <- declare_assignment(clusters = villages)
  assignment_11 <- declare_assignment(clusters = villages, condition_names = c(0, 1))
  assignment_12 <- declare_assignment(clusters = villages, prob_each = c(.1, .3, .6), reveal=FALSE)

  # Blocked and Clustered assignments
  assignment_13 <- declare_assignment(clusters = villages,
                                      blocks = high_elevation)

  assignment_14 <- declare_assignment(clusters = villages,
                                      blocks = high_elevation, condition_names = c(0,1))
  assignment_15 <- declare_assignment(clusters = villages,
                                      blocks = high_elevation, prob_each = c(.1, .3, .6), reveal=FALSE)

  # Draw Data
  smp_draw <- population() %>% sampling() %>% potential_outcomes()

  smp_draw %>% head
  # Attempt to Assign

  smp_draw %>% assignment_0() %>% with(table(Z))
  smp_draw %>% assignment_1() %>% with(table(Z))
  smp_draw %>% assignment_2() %>% with(table(Z))
  smp_draw %>% assignment_3() %>% with(table(Z))
  smp_draw %>% assignment_4() %>% with(.,table(Z))
  smp_draw %>% assignment_5() %>% with(.,table(Z))
  smp_draw %>% assignment_6() %>% with(.,table(ideo_3, Z))
  smp_draw %>% assignment_7() %>% with(.,table(ideo_3, Z))
  smp_draw %>% assignment_8() %>% with(.,table(ideo_3, Z))
  smp_draw %>% assignment_9() %>% with(.,table(ideo_3, Z))
  smp_draw %>% assignment_10() %>% with(.,table(villages, Z))
  smp_draw %>% assignment_11() %>% with(.,table(villages, Z))
  smp_draw %>% assignment_12() %>% with(.,table(villages, Z))
  smp_draw %>% assignment_13() %>% with(.,table(villages, Z))
  smp_draw %>% assignment_14() %>% with(.,table(villages, Z))
  smp_draw %>% assignment_15() %>% with(.,table(villages, Z))

  # Obtain Treatment Probabilities
  smp_draw %>% assignment_0() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_1() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_2() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_3() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_4() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_5() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_6() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_7() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_8() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_9() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_10() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_11() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_12() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_13() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_14() %>% .$Z_cond_prob %>% head()
  smp_draw %>% assignment_15() %>% .$Z_cond_prob %>% head()

})

test_that("declare_assignment expected failures via validation fn", {

  expect_true(is.function(declare_assignment()))

  expect_error(declare_assignment(blocks='character'), "blocks")

  expect_error(declare_assignment(clusters='character'), "clusters")

  expect_error(declare_assignment(assignment_variable_name = NULL), "assignment_variable_name")
})


