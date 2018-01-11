context("Estimators")

test_that("test the estimators", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  my_assignment <- declare_assignment(m = 25)

  ## difference in means
  my_estimator <- declare_estimator(Y ~ Z)
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## lm with robust ses
  my_estimator <- declare_estimator(Y ~ Z, model = lm_robust)
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## lm with HC3 robust ses
  my_estimator <- declare_estimator(Y ~ Z, model = lm_robust, se_type = "HC3")
  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator

  ## custom estimator function
  my_estimator_function <- function(formula, data){
    data.frame(est = with(data, mean(Y)))
  }

  my_estimator_custom <-
    declare_estimator(Y ~ Z, estimator_function = my_estimator_function)

  my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes %>% my_estimator_custom


  ## check blocked d-i-m estimator

  my_population <- declare_population(N = 500, noise = rnorm(N), blocks = sample(rep(c("A", "B"), each = 250), N, replace = F))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2) + 5 * (blocks == "A"))
  my_assignment <- declare_assignment(blocks = blocks)

  ## lm with HC3 robust ses
  my_estimator_blocked <- declare_estimator(Y ~ Z, model = difference_in_means, blocks = `blocks`)
  df <- my_population() %>% my_potential_outcomes %>% my_assignment %>% reveal_outcomes
  my_estimator_notblocked <- declare_estimator(Y ~ Z)

  df %>% my_estimator_notblocked
  df %>% my_estimator_blocked ## it is different!

})



test_that("regression from estimatr works as an estimator", {

  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  my_assignment <- declare_assignment(m = 100)
  pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
  pate_estimator <- declare_estimator(Y ~ Z + noise,
                                      model = lm_robust,
                                      coefficient_name = "noise",
                                      estimand = pate, label = "pate")

  my_design <- declare_design(my_population,
                              my_potential_outcomes,
                              pate,
                              my_assignment,
                              reveal_outcomes,
                              pate_estimator)

  execute_design(my_design)

  diagnosis <- diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = FALSE)
  diagnosis

})



test_that("multiple estimator declarations work", {

  population <- declare_population(
    N = 200,
    noise = rnorm(N)
  )

  potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ noise + Z*.5)

  pop <- potential_outcomes(population())


  sampling <- declare_sampling(
    n = 100
  )


  assignment <- declare_assignment(
    N = 50
  )

  sate <- declare_estimand(SATE = mean(Y_Z_1 - Y_Z_0))

  estimator_1 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate,
                      label = "estimator_1")

  estimator_2 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate,
                      label = "estimator_2")

  declare_design(population,
                 potential_outcomes,
                 sampling,
                 sate,
                 assignment,
                 reveal_outcomes,
                 estimator_1,
                 estimator_2
  )

  estimator_3 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate,
                      label = "estimator_3")

  estimator_4 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate,
                      label = "estimator_4")

  declare_design(population,
                           potential_outcomes,
                           sampling,
                           sate,
                           assignment,
                           reveal_outcomes,
                           estimator_3,
                           estimator_4
  )


  estimator_5 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate)

  estimator_6 <-
    declare_estimator(formula = Y ~ Z,
                      estimator_function = estimatr::lm_robust,
                      estimand = sate)

  # This could eventually be fixed so that the estimator names are inherited
  expect_error({
    design <- declare_design(population,
                             potential_outcomes,
                             sampling,
                             sate,
                             assignment,
                             reveal_outcomes,
                             estimator_5,
                             estimator_6
    )
  }
  )



})


test_that("labels for estimates and estimands work", {

# no mands, one mator,
# one mand one mator,
# two mands one mator,
#
# no mands, two mators,
# one mand two mators,
# two mands two mators

  my_population <- declare_population(N = 50, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_assignment <- declare_assignment(m = 25)

  mand_arg_label <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  mand_explicit_label <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "the_ATE")
  # mand_explicit_label_noquote <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = the_ATE)

  mator_no_label <- declare_estimator(Y ~ Z, estimand = mand_arg_label)
  mator_label <- declare_estimator(Y ~ Z, estimand = mand_arg_label, label = "an_estimator")
  # mator_label_noquote <- declare_estimator(Y ~ Z, estimand = mand_arg_label, label = an_estimator)
  mator_label_null <- declare_estimator(Y ~ Z, estimand = mand_arg_label, label = NULL)

  mator_no_label <- declare_estimator(Y ~ Z, estimand = mand_explicit_label)
  mator_label <- declare_estimator(Y ~ Z, estimand = mand_explicit_label, label = "an_estimator")
  # mator_label_noquote <- declare_estimator(Y ~ Z, estimand = mand_explicit_label, label = an_estimator)
  mator_label_null <- declare_estimator(Y ~ Z, estimand = mand_explicit_label, label = NULL)

  mator_no_label <- declare_estimator(Y ~ Z, estimand = mand_explicit_label)
  mator_label <- declare_estimator(Y ~ Z, estimand = mand_explicit_label_noquote, label = "an_estimator")
  # mator_label_noquote <- declare_estimator(Y ~ Z, estimand = mand_explicit_label_noquote, label = an_estimator)
  mator_label_null <- declare_estimator(Y ~ Z, estimand = mand_explicit_label_noquote, label = NULL)

  design <- declare_design(my_population,
                           my_potential_outcomes,
                           mand_arg_label,
                           my_assignment,
                           reveal_outcomes,
                           mator_no_label)

  diagnose_design(    design, sims = 2, bootstrap = FALSE, parallel = FALSE)
})


test_that("coefficient_name = NULL returns all coefficients", {
  tst <- data.frame(x = runif(100), y = runif(100), wt = runif(100), clust = sample(1:10, 100, replace = TRUE))

  est4 <- declare_estimator(
      y ~ x + as.factor(clust),
      clusters = clust,
      weights = wt,
      model = lm_robust,
      coefficient_name = NULL)

  result <- est4(tst)

  expect_gt(nrow(result), 0)
})

