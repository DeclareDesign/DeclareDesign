

context("get_estimates")

test_that("get_estimates works", {
  
  set.seed(25)
  
  N <- 500
  
  my_population <- declare_population(data = sleep)
  
  my_estimator <- declare_estimator(extra ~ group)
  
  design <- my_population +
    my_estimator + 
    declare_estimator(extra ~ group, model = lm_robust, label = "est2")
  
  expect_equal(draw_estimates(design),
               structure(list(estimator_label = c("estimator", "est2"), term = c("group2", 
                                                                                 "group2"), estimate = c(1.58, 1.58), std.error = c(0.849091017238762, 
                                                                                                                                    0.849091017238762), statistic = c(1.86081346748685, 1.86081346748685
                                                                                                                                    ), p.value = c(0.0793941401873581, 0.0791867142159381), conf.low = c(-0.205483230711711, 
                                                                                                                                                                                                         -0.203874032287598), conf.high = c(3.36548323071171, 3.3638740322876
                                                                                                                                                                                                         ), df = c(17.7764735161785, 18), outcome = c("extra", "extra"
                                                                                                                                                                                                         )), row.names = c(NA, -2L), class = "data.frame"))
  
  expect_equal(get_estimates(design, data = draw_data(design)),
               structure(list(estimator_label = c("estimator", "est2"), term = c("group2", 
                                                                                 "group2"), estimate = c(1.58, 1.58), std.error = c(0.849091017238762, 
                                                                                                                                    0.849091017238762), statistic = c(1.86081346748685, 1.86081346748685
                                                                                                                                    ), p.value = c(0.0793941401873581, 0.0791867142159381), conf.low = c(-0.205483230711711, 
                                                                                                                                                                                                         -0.203874032287598), conf.high = c(3.36548323071171, 3.3638740322876
                                                                                                                                                                                                         ), df = c(17.7764735161785, 18), outcome = c("extra", "extra"
                                                                                                                                                                                                         )), row.names = c(NA, -2L), class = "data.frame"))
  
  expect_equal(get_estimates(design, data = draw_data(design), start = 3),
               structure(list(estimator_label = "est2", term = "group2", estimate = 1.58, 
                              std.error = 0.849091017238762, statistic = 1.86081346748685, 
                              p.value = 0.0791867142159381, conf.low = -0.203874032287598, 
                              conf.high = 3.3638740322876, df = 18, outcome = "extra"), row.names = c(NA, 
                                                                                                      -1L), class = "data.frame"))
               
})
  

test_that("warning for wrong class estimator label", {
  
  my_population <- declare_population(data = sleep)
  
  custom_func <- function(data){
    data.frame(
      est = 5, estimator_label = "test"
    )
  }
  
  my_estimator <- declare_estimator(handler = custom_func)
  
  design <- my_population + my_estimator 
  
  expect_warning(draw_estimates(design), "The estimator label should be a character")
  
  custom_func <- function(data){
    data.frame(
      est = 5, estimand_label = "test"
    )
  }
  
  my_estimand <- declare_estimand(handler = custom_func)
  
  design <- my_population + my_estimand 
  
  expect_warning(draw_estimands(design), "The estimand label should be a character")
  
})


