context("diagnose design")

test_that("error when you send other objects to diagnose", {

  # must send a function or a design object
  expect_error(diagnose_design(rep(3, 2)), "Please only send design objects or functions with no arguments.")
})


test_that("default diagnosands work", {
  my_designer <- function(N = 10) {
    my_population <- declare_population(N = N, noise = rnorm(N))

    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

    my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

    my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

    my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

    my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

    design <- my_population +
      my_potential_outcomes +
      my_inquiry +
      declare_step(dplyr::mutate, q = 5) +
      my_assignment +
      my_measurement +
      my_estimator

    diagnosands <- declare_diagnosands(med_bias = median(estimate - estimand))

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
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(2, 9))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "diagnosand", "estimate", 
                                    "std.error", "conf.low", "conf.high"))
  
  expect_equal(names(diag$diagnosands_df), 
               c("design", "inquiry", "estimator", "term", 
                 "med_bias", "se(med_bias)", "n_sims"
               ))
  
  # w/ set diagnosands each manually

  design_1 <- set_diagnosands(my_designer(N = 100), NULL)
  design_2 <- set_diagnosands(my_designer(N = 200), NULL)

  diagnosand_1 <- declare_diagnosands(my_bias = median(estimate - estimand))
  diagnosand_2 <- declare_diagnosands(my_power = mean(p.value <= .5))

  # intentionally out of order to confirm they don't get mixed
  diag <- diagnose_design(
    design_2 = set_diagnosands(design_2, diagnosand_2),
    design_1 = set_diagnosands(design_1, diagnosand_1),
    sims = 2
  )
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(4, 9))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "diagnosand", "estimate", 
                                    "std.error", "conf.low", "conf.high"))
  
  expect_equal(names(diag$diagnosands_df), c("design", "inquiry", "estimator", "term", 
                                             "my_bias", "se(my_bias)", "my_power", 
                                             "se(my_power)", "n_sims"))
  
  # w/ none set

  diag <- diagnose_design(
    design_2 = design_2,
    design_1 = design_1,
    sims = 2
  )
  
  expect_equal(names(diag$diagnosands_df), 
               c("design", "inquiry", "estimator", "term", "mean_estimand", 
                 "se(mean_estimand)", "mean_estimate", "se(mean_estimate)", "bias", 
                 "se(bias)", "sd_estimate", "se(sd_estimate)", "rmse", "se(rmse)", 
                 "power", "se(power)", "coverage", "se(coverage)", "n_sims"))
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(14, 9))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "diagnosand", "estimate", 
                                    "std.error", "conf.low", "conf.high"))
  
  # w/ none set and override

  diag <- diagnose_design(
    design_2 = design_2,
    design_1 = design_1,
    diagnosands = declare_diagnosands(med_bias = median(estimate - estimand)),
    sims = 2
  )
    
  expect_equal(names(diag$diagnosands_df), 
               c("design", "inquiry", "estimator", "term", 
                 "med_bias", "se(med_bias)", "n_sims"
               ))
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(2, 9))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "diagnosand", "estimate", 
                                    "std.error", "conf.low", "conf.high"))
  
  # w/ mix of set and unset
  
  # // expand_designs list

  # w/ diagnosands set

  designs <- expand_design(my_designer, N = c(100, 200))

  diag <- diagnose_design(designs, sims = 5, bootstrap_sims = FALSE)
 
  expect_equal(names(diag$diagnosands_df), 
               c("design", "N", "inquiry", "estimator", "term", 
                 "med_bias", "n_sims"))
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(2, 7))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "N", "diagnosand", "estimate"))
  
  # w mix of diagnosands set

  attr(designs[[1]], "diagnosands") <- NULL

  diag <- diagnose_design(designs, sims = 5, bootstrap_sims = FALSE)
  
  expect_equal(ncol(diag$diagnosands_df), 14)
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(16, 7))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "N", "diagnosand", "estimate"))
  
  # // simulation df
  sims <- set_diagnosands(simulate_design(designs, sims = 5), declare_diagnosands(med_bias = median(estimate - estimand)))
  diag <- diagnose_design(sims, sims = 5, bootstrap_sims = FALSE)
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diag)), c(2, 7))
  expect_equal(names(tidy(diag)), c("design", "inquiry", "estimator", "term", "N", "diagnosand", "estimate"))
})


test_that("more term",{
  population  <-
    declare_model(N = 100,
                       Z = rep(0:1, 50),
                       Y = rnorm(N))
  
  inquiries_regression <- declare_inquiry(
    `(Intercept)` = 0,
    `Z` = 1,
    term = TRUE,
    label = "Regression_Inquiries"
  )
  
  estimators_regression <- declare_estimator(Y ~ Z,
                                             inquiry = inquiries_regression,
                                             model = lm_robust,
                                             term = TRUE)
  
  inquiry_2  <- declare_inquiry(ATE = 2,   label = "2")
  estimator_2 <-
    declare_estimator(Y ~ Z, inquiry = inquiry_2, label = "dim")
  
  design <-
    population + inquiries_regression + estimators_regression + inquiry_2 + estimator_2
  
  sims_df <- simulate_design(design, sims = 1)
  
  expect_equal(nrow(sims_df), 3)
  
  expect_equal(sims_df[, 1:6],
               structure(
                 list(
                   design = c("design", "design", "design"),
                   sim_ID = c(1L, 1L, 1L),
                   inquiry = c("ATE", "Regression_Inquiries",
                                      "Regression_Inquiries"),
                   estimand = c(2, 0, 1),
                   estimator = c("dim",
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
    declare_model(N = 5, noise = rnorm(N)) +
       declare_inquiry(mean_noise = mean(noise))
  
  designs <- redesign(design, N = 5:10) 
  dx <- diagnose_design(designs, sims = 50, bootstrap_sims = FALSE)
  
  expect_equal(class(dx$simulations_df$N), "integer") 
  expect_equal(class(dx$diagnosands_df$N), "integer")
  
  # test tidy.diagnosis
  expect_equal(class(tidy(dx)$N), "integer") 
  
  # works for expand_design
  designer <- function(N = 5) {
    declare_model(N = N, noise = rnorm(N)) +
    declare_inquiry(mean_noise = mean(noise))
  }
  
  designs <- expand_design(designer, N = 5:10) 
  dx <- diagnose_design(designs, sims = 50, bootstrap_sims = FALSE)
  
  expect_equal(class(dx$simulations_df$N), "integer") 
  expect_equal(class(dx$diagnosands_df$N), "integer")
  
  # test tidy.diagnosis
  expect_equal(class(tidy(dx)$N), "integer") 
  
})


test_that("diagnose_design works when simulations_df lacking parameters attr", {

  design <- declare_model(N = 100, X = rnorm(N), Y = rnorm(N, X)) +
    declare_inquiry(true_effect = 1) +
    declare_estimator(Y ~ X, model=lm_robust, inquiry = "true_effect", label = "Y on X") 
  
  simulations <-  simulate_design(design, sims = 20) 
  
  simulations_no_attr <- simulations
  attributes(simulations_no_attr)["parameters"] <- NULL
  
  d1 <- diagnose_design(simulations, bootstrap_sims = FALSE)
  d2 <- diagnose_design(simulations_no_attr, bootstrap_sims = FALSE)
  
  # strip params from d1
  # These are expected differences
  attributes(d1$simulations_df)["parameters"] <- NULL
  d1$simulations_df$design <- as.character(d1$simulations_df$design)
  d1$diagnosands_df$design <- as.character(d1$diagnosands_df$design)
  d1["parameters_df"] <- list(NULL)
  d1$duration <- NULL
  d2$duration <- NULL # dropped because durations will always be slightly different
    
  expect_identical(d1,d2)
  
  # test tidy.diagnosis
  expect_identical(tidy(d1), tidy(d2))
  
})


test_that("diagnose_design stops when a zero-row simulations_df is sent", {
  expect_error(diagnose_design(data.frame(estimator = rep(1, 0))), "which has zero rows")
})

test_that("diagnose_design can generate and use grouping variables", {
  
  set.seed(5)
  
  design <- 
    declare_model(N = 100, u = rnorm(N),
      Y_Z_0 = 0, 
      Y_Z_1 = ifelse(rbinom(N, 1, prob = 0.5), 0.1, -0.1) + u
    ) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_inquiry(ATE_positive = mean(Y_Z_1 - Y_Z_0) > 0) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE_positive")
  
  diagnosis <- diagnose_design(design, 
                               make_groups = vars(estimand, significant = p.value <= 0.05),
                               sims = 5
  )
  expect_equivalent(diagnosis$diagnosands_df$significant,  c(FALSE, FALSE))
  expect_equivalent(diagnosis$diagnosands_df$estimand,  c(FALSE, TRUE))
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diagnosis)), c(14, 11))
  expect_equal(names(tidy(diagnosis)), c("design", "inquiry", "estimator", "term", "estimand", "significant", "diagnosand", "estimate", 
                                         "std.error", "conf.low", "conf.high"))
  
  design <- 
    declare_model(N = 100, u = rnorm(N),
      Y_Z_0 = 0, 
      Y_Z_1 = ifelse(rbinom(N, 1, prob = 0.5), 0.1, -0.1) + u
    ) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")
  
  diagnosis <- diagnose_design(
    design,
    make_groups = vars(effect_size = cut(
      estimand, quantile(estimand, (0:4) / 4), include.lowest = TRUE
    )),
    sims = 5
  )
  
  expect_equivalent(nrow(diagnosis$diagnosands_df),  4)
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diagnosis)), c(28, 10))
  expect_equal(names(tidy(diagnosis)), c("design", "inquiry", "estimator", "term", "effect_size", "diagnosand", 
                                         "estimate", "std.error", "conf.low", "conf.high"))
  
  
  design <- 
    declare_model(N = 100, u = rnorm(N),
      Y_Z_0 = 0, 
      Y_Z_1 = ifelse(rbinom(N, 1, prob = 0.5), 0.1, -0.1) + u
    ) +
    declare_assignment(Z = complete_ra(N)) + 
    declare_inquiry(ATE_positive = mean(Y_Z_1 - Y_Z_0) > 0) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE_positive")
  
  diagnosis <- diagnose_design(design, 
                               make_groups = vars(significant = ifelse(p.value > 0.1, NA, p.value <= 0.05)),
                               sims = 5
  )
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diagnosis)), c(14, 10))
  expect_equal(names(tidy(diagnosis)), c("design", "inquiry", "estimator", "term", "significant", "diagnosand", 
                                         "estimate", "std.error", "conf.low", "conf.high"))
  
  print(diagnosis, digits = 5, select = "Bias")
  
  diagnosis <- diagnose_design(design, 
                               make_groups = vars(significant = factor(ifelse(p.value > 0.1, NA, p.value <= 0.05))),
                               sims = 100
  )
  
  # test tidy.diagnosis
  expect_equal(dim(tidy(diagnosis)), c(21, 10))
  expect_equal(names(tidy(diagnosis)), c("design", "inquiry", "estimator", "term", "significant", "diagnosand", 
                                         "estimate", "std.error", "conf.low", "conf.high"))
  
  
})


test_that("tidy.diagnosis handles NAs", {
  
  design <- 
    declare_model(N = 10, Y = rnorm(N)) +
    declare_estimator(Y ~ 1, label = "normal") +
    declare_estimator(handler = function(data){data.frame(estimate = 5)})
  
  dx <- diagnose_design(design, sims = 5)
  
  expect_equal(dim(tidy(dx)), c(14, 8))
  
  set.seed(343)
  portola <-
    fabricate(
      N = 2100,
      Y_star = rnorm(N)
    )
  
  design <- 
    declare_model(data = portola) + 
    declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
    declare_inquiry(Y_bar = mean(Y)) + 
    declare_sampling(S = complete_rs(N, n = 100)) + 
    declare_measurement(
      R = rbinom(n = N, size = 1, prob = pnorm(Y_star + effort)),
      Y = ifelse(R == 1, Y, NA_real_)
    ) +
    declare_estimator(Y ~ 1, inquiry = "Y_bar") +
    declare_estimator(R ~ 1, label = "Response Rate")
  
  designs <- redesign(design, effort = seq(0, 5, by = 1))
  diagnosis <-
    diagnose_designs(
      designs,
      sims = 20,
      bootstrap_sims = 20
    )
  
  # checks a bug involved in sorting that led to NAs in mean estimate (possibly related to the case where you have parameters?)
  expect_equal(sum(is.na(tidy(diagnosis) %>% dplyr::filter(diagnosand == "mean_estimate", estimator == "estimator") %>% dplyr::pull(estimate))), 0)
  
})

