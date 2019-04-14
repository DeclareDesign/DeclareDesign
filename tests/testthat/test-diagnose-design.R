context("diagnose design")

test_that("allow design functions to be sent to simulate design and diagnose_design", {
  my_design_function <- function() {
    N <- 500

    my_population <- declare_population(N = N, noise = rnorm(N))

    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_sampling <- declare_sampling(n = 250)

    my_assignment <- declare_assignment(m = 25)

    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

    my_reveal <- declare_reveal()

    design <- my_population +
      my_potential_outcomes +
      my_sampling +
      my_estimand +
      declare_step(dplyr::mutate, q = 5) +
      my_assignment +
      my_reveal +
      my_estimator

    run_design(design)
  }

  des_out <- run_design(my_design_function)

  expect_equal(names(des_out), c("estimates_df", "estimands_df"))

  sims_out <- simulate_design(my_design_function, sims = 2)

  expect_equal(
    sims_out[, 1:2],
    structure(list(design_label = c("my_design_function", "my_design_function"), sim_ID = 1:2), class = "data.frame", row.names = c(NA, -2L))
  )

  diag_out <- diagnose_design(my_design_function, sims = 2, bootstrap_sims = FALSE)


  expect_equal(
    diag_out$diagnosands_df[, 1:4],
    structure(list(
      design_label = structure(1L, .Label = "my_design_function", class = "factor"),
      estimand_label = "ATE", estimator_label = "estimator",
      term = "Z"
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    ))
  )
})


test_that("error when you send other objects to diagnose", {

  # must send a function or a design object
  expect_error(diagnose_design(rep(3, 2)), "Please only send design objects or functions with no arguments.")
})


test_that("default diagnosands work", {
  my_designer <- function(N = 500) {
    my_population <- declare_population(N = N, noise = rnorm(N))

    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_assignment <- declare_assignment(m = 25)

    my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

    my_reveal <- declare_reveal()

    design <- my_population +
      my_potential_outcomes +
      my_estimand +
      declare_step(dplyr::mutate, q = 5) +
      my_assignment +
      my_reveal +
      my_estimator

    diagnosands <- declare_diagnosands(med_bias = median(estimate - estimand), keep_defaults = FALSE)

    set_diagnosands(design, diagnosands)
  }

  # five cases

  # designs

  # // single design

  # w/ set diagnosands

  # w/o set diagnosands

  # // ... of designs

  design_1 <- my_designer(N = 100)
  design_2 <- my_designer(N = 200)

  # w/ set diagnosands outside diagnose_design

  diag <- diagnose_design(
    design_2 = design_2,
    design_1 = design_1,
    sims = 2
  )

  expect_equal(names(diag$diagnosands_df), 
               c("design_label", "estimand_label", "estimator_label", "term", 
                 "med_bias", "se(med_bias)", "n_sims"
               ))
  
  # w/ set diagnosands each manually

  design_1 <- set_diagnosands(my_designer(N = 100), NULL)
  design_2 <- set_diagnosands(my_designer(N = 200), NULL)

  diagnosand_1 <- declare_diagnosands(my_bias = median(estimate - estimand), keep_defaults = FALSE)
  diagnosand_2 <- declare_diagnosands(my_power = mean(p.value <= .5), keep_defaults = FALSE)

  # intentionally out of order to confirm they don't get mixed
  diag <- diagnose_design(
    design_2 = set_diagnosands(design_2, diagnosand_2),
    design_1 = set_diagnosands(design_1, diagnosand_1),
    sims = 2
  )
  
  expect_equal(names(diag$diagnosands_df), c("design_label", "estimand_label", "estimator_label", "term", 
                                             "my_bias", "se(my_bias)", "my_power", 
                                             "se(my_power)", "n_sims"))
  
  # w/ none set

  diag <- diagnose_design(
    design_2 = design_2,
    design_1 = design_1,
    sims = 2
  )
  
  expect_equal(names(diag$diagnosands_df), 
               c("design_label", "estimand_label", "estimator_label", "term", 
                 "bias", "se(bias)", "rmse", "se(rmse)", "power", "se(power)", 
                 "coverage", "se(coverage)", "mean_estimate", "se(mean_estimate)", 
                 "sd_estimate", "se(sd_estimate)", "mean_se", "se(mean_se)", "type_s_rate", 
                 "se(type_s_rate)", "mean_estimand", "se(mean_estimand)", "n_sims"))
  
  # w/ none set and override

  diag <- diagnose_design(
    design_2 = design_2,
    design_1 = design_1,
    diagnosands = declare_diagnosands(med_bias = median(estimate - estimand), keep_defaults = FALSE),
    sims = 2
  )
    
  expect_equal(names(diag$diagnosands_df), 
               c("design_label", "estimand_label", "estimator_label", "term", 
                 "med_bias", "se(med_bias)", "n_sims"
               ))
  
  
  # w/ mix of set and unset
  
  # // expand_designs list

  # w/ diagnosands set

  designs <- expand_design(my_designer, N = c(100, 200))

  diag <- diagnose_design(designs, sims = 5, bootstrap_sims = FALSE)
 
  expect_equal(names(diag$diagnosands_df), 
               c("design_label", "N", "estimand_label", "estimator_label", "term", 
                 "med_bias", "n_sims"))
  
  # w mix of diagnosands set

  attr(designs[[1]], "diagnosands") <- NULL

  diag <- diagnose_design(designs, sims = 5, bootstrap_sims = FALSE)
  
  expect_equal(ncol(diag$diagnosands_df), 16)
  
  # // simulation df
  sims <- set_diagnosands(simulate_design(designs, sims = 5), declare_diagnosands(med_bias = median(estimate - estimand)))
  diag <- diagnose_design(sims, sims = 5, bootstrap_sims = FALSE)
})


test_that("more term",{
  population  <-
    declare_population(N = 100,
                       Z = rep(0:1, 50),
                       Y = rnorm(N))
  
  estimands_regression <- declare_estimand(
    `(Intercept)` = 0,
    `Z` = 1,
    term = TRUE,
    label = "Regression_Estimands"
  )
  
  estimators_regression <- declare_estimator(Y ~ Z,
                                             estimand = estimands_regression,
                                             model = lm_robust,
                                             term = TRUE)
  
  estimand_2  <- declare_estimand(ATE = 2,   label = "2")
  estimator_2 <-
    declare_estimator(Y ~ Z, estimand = estimand_2, label = "dim")
  
  design <-
    population + estimands_regression + estimators_regression + estimand_2 + estimator_2
  
  sims_df <- simulate_design(design, sims = 1)
  
  expect_equal(nrow(sims_df), 3)
  
  expect_equal(sims_df[, 1:6],
               structure(
                 list(
                   design_label = c("design", "design", "design"),
                   sim_ID = c(1L, 1L, 1L),
                   estimand_label = c("ATE", "Regression_Estimands",
                                      "Regression_Estimands"),
                   estimand = c(2, 0, 1),
                   estimator_label = c("dim",
                                       "estimator", "estimator"),
                   term = c("Z", "(Intercept)", "Z")
                 ),
                 class = "data.frame",
                 row.names = c(NA,-3L)
               ))
  
  
  
})

test_that("diagnose_design does not reclass the variable N", {
  skip_if(compareVersion("3.5", paste(R.Version()$major, R.Version()$minor, sep = ".")) == 1)
  # works for redesign
  design <-
    declare_population(N = 5, noise = rnorm(N)) +
       declare_estimand(mean_noise = mean(noise))
  
  designs <- redesign(design, N = 5:10) 
  dx <- diagnose_design(designs, sims = 50, bootstrap_sims = FALSE)
  
  expect_equal(class(dx$simulations_df$N), "integer") 
  expect_equal(class(dx$diagnosands_df$N), "integer")
  
  # works for expand_design
  designer <- function(N = 5) {
    declare_population(N = N, noise = rnorm(N)) +
    declare_estimand(mean_noise = mean(noise))
  }
  
  designs <- expand_design(designer, N = 5:10) 
  dx <- diagnose_design(designs, sims = 50, bootstrap_sims = FALSE)
  
  expect_equal(class(dx$simulations_df$N), "integer") 
  expect_equal(class(dx$diagnosands_df$N), "integer")
  
})


test_that("diagnose_design works when simulations_df lacking parameters attr", {

  design <- declare_population(N = 100, X = rnorm(N), Y = rnorm(N, X)) +
    declare_estimand(true_effect = 1) +
    declare_estimator(Y ~ X, model=lm_robust, estimand = "true_effect", label = "Y on X") 
  
  simulations <-  simulate_design(design, sims = 20) 
  
  simulations_no_attr <- simulations
  attributes(simulations_no_attr)["parameters"] <- NULL
  
  d1 <- diagnose_design(simulations, bootstrap_sims = FALSE)
  d2 <- diagnose_design(simulations_no_attr, bootstrap_sims = FALSE)
  
  # strip params from d1
  # These are expected differences
  attributes(d1$simulations_df)["parameters"] <- NULL
  d1$simulations_df$design_label <- as.character(d1$simulations_df$design_label)
  d1$diagnosands_df$design_label <- as.character(d1$diagnosands_df$design_label)
  d1["parameters_df"] <- list(NULL)
    
  expect_identical(d1,d2)
})
