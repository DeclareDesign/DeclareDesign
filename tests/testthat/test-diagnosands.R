context("Diagnosands")

my_population <- declare_population(N = 50, noise = rnorm(N))
fixed_pop <- my_population()
my_pop <- declare_population(fixed_pop)

my_potential_outcomes <-
  declare_potential_outcomes(
    Y_Z_0 = noise,
    Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
  )

my_assignment <- declare_assignment(m = 25)

pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")

reveal_outcomes <- reveal_outcomes()

my_design <- my_pop +
  my_potential_outcomes +
  pate +
  my_assignment +
  reveal_outcomes +
  pate_estimator

my_design_2 <- my_design
my_design_3 <- my_design


test_that("s3 dispatch works", {
  diagnosis <- diagnose_design(my_design, sims = 5, bootstrap_sims = FALSE)
  diagnosis <- diagnose_design(my_design, sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diagnosis$diagnosands_df), 1)

  diagnosis <- diagnose_design(my_design, my_design_2,
    sims = list(my_design = 30, my_design_2 = 31),
    bootstrap_sims = FALSE
  )

  expect_equal(nrow(diagnosis$diagnosands_df), 2)
  expect_true(all(diagnosis$diagnosands_df$n_sims %in% c(30, 31)))

  diagnosis <- diagnose_design(list(my_design, my_design_2), sims = 5, bootstrap_sims = FALSE)
  expect_equal(nrow(diagnosis$diagnosands_df), 2)

  expect_error(diagnose_design(list(my_design, my_design_2), my_design_3, sims = 5, bootstrap_sims = FALSE))

  sims_df <- simulate_design(my_design, my_design_2, sims = 5)
  diagnosis <- diagnose_design(sims_df)

  expect_equal(nrow(diagnosis$diagnosands_df), 2)
})

test_that("parallel works.", {
  # TODO use future

  skip_if_not_installed("future.apply")
  skip_on_cran()

  suppressWarnings(
    diag <- diagnose_design(my_design, sims = 2, bootstrap_sims = FALSE)
  )

  expect_output(print(diag), regexp = "Research design diagnosis")
})

test_that("Diagnosis prints ok", {
  diag <- diagnose_design(my_design, sims = 2, bootstrap_sims = FALSE)

  ## diagnose_design(my_design, sims = 2, bootstrap_sims = FALSE, parallel = TRUE)

  expect_output(print(diag), regexp = "Research design diagnosis")
})


test_that("test diagnosands without estimands", {
  my_design2 <- my_population +
    my_potential_outcomes +
    my_assignment +
    reveal_outcomes +
    declare_estimator(Y ~ Z)

  my_dig <- declare_diagnosands(mean_est = mean(estimate), sd_est = sd(estimate))
  diagnosis <- diagnose_design(my_design2, sims = 2, diagnosands = my_dig, bootstrap_sims = FALSE)


  expect_equal(dim(diagnosis$diagnosands_df), c(1,6))

})


test_that("custom diagnosand function", {
  mean_custom <- function(x) return(mean(x * 5))
  my_dig <- declare_diagnosands(mean_x5 = mean_custom(estimate), mean_true = mean(estimate))

  rm(mean_custom)
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap_sims = FALSE)
  expect_true("mean_x5" %in% names(diagnosis$diagnosands))

  # works with two with bootstrapping
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap_sims = 2)
  expect_true("se(mean_x5)" %in% names(diagnosis$diagnosands))
})


test_that("single diagnosand function", {
  # works with only one diagnosand with bootstrapping (!)
  my_one_dig <- declare_diagnosands(se_bias = mean(std.error - sd(estimand)))
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_one_dig, bootstrap_sims = 5)

  expect_true("se_bias" %in% names(diagnosis$diagnosands))
})


test_that("no estimates, no estimators should error", {
  my_population <- declare_population(N = 50)

  my_design <- my_population + NULL

  head(draw_data(my_design))
  expect_error(diagnose_design(my_population, sims = 2, bootstrap_sims = FALSE))
})





test_that("diagnosis, list of designs", {
  d <- declare_population(sleep) +
    declare_estimator(extra ~ group, term = group2)

  diagnosand <- declare_diagnosands(z = mean(estimate > 0))

  expect_error(diagnose_design(sleep), "Can't calculate diagnosands on this data.frame, which does not include either an estimator_label or an estimand_label. Did you send a simulations data frame?")

  diag1 <- diagnose_design(list(d, d), diagnosands = diagnosand, sims = 5, bootstrap_sims = FALSE)
  diag2 <- diagnose_design(design_1 = d, design_2 = d, diagnosands = diagnosand, sims = 5, bootstrap_sims = FALSE)

  expect_identical(diag1, diag2)
})

test_that("diagnosis, unlinked estimator", {
  d <- declare_population(sleep) +
    declare_estimand(foo = 2, bar = 3) +
    declare_estimator(extra ~ group, model = lm, term = TRUE)
  expect_warning(diagnose_design(d, sims = 5, bootstrap_sims = FALSE), "Estimators lack estimand/term labels for matching, a many-to-many merge was performed.")
})


test_that("diagnosis, no estimator", {
  d <- declare_population(sleep) +
    declare_estimand(foo = 2, bar = 3)

  diagnosand <- declare_diagnosands(z = mean(estimand > 0))
 

  expect_equivalent(
    diagnose_design(
      d,
      diagnosands = diagnosand,
      sims = 5,
      bootstrap_sims = 5
    )$diagnosands_df,
    structure(list(design_label = structure(c(1L, 1L), .Label = "d", class = "factor"), 
                   estimand_label = c("bar", "foo"), z = c(1, 1), `se(z)` = c(0, 
                                                                              0), n_sims = c(5L, 5L)), class = "data.frame", row.names = c(NA, 
                                                                                                                                           -2L)))
})

test_that("Overriding join conditions", {
  skip_if_not_installed("reshape2")
  skip_if_not_installed("dplyr")

  require(dplyr)

  alpha <- .05

  custom <- declare_diagnosands(handler = function(data) {
    data %>%
      group_by(sim_ID) %>%
      summarize(
        any_significant = any(p.value < alpha),
        num_significant = sum(p.value < alpha),
        all_significant = all(p.value < alpha)
      ) %>%
      summarize(
        any_significant = mean(any_significant),
        num_significant = mean(num_significant),
        all_significant = mean(all_significant)
      ) %>%
      reshape2::melt(id.vars = NULL, variable.name = "estimand_label", value.name = "estimand")
  })

  attr(custom, "group_by") <- c("estimand_label", "estimator_label")

  design <- declare_population(data=sleep, handler = fabricatr::resample_data) +
    declare_estimand(group1 = 1, group2 = 2, term = TRUE, label = "e") +
    declare_estimator(extra ~ group + 0, term = TRUE, estimand = "e", model = lm, label = "my_estimator")

  diagnosands <- get_diagnosands(diagnose_design(design, diagnosands = custom, sims = 5, bootstrap_sims = FALSE))

  expect_true(is.data.frame(diagnosands) && nrow(diagnosands) == 2)
})

test_that("diagnosis, NAs if no estimand", {
  ols <- declare_estimator(extra ~ group)
  d <- declare_population(sleep) + ols
  
  sleep_ols <- structure(list(design_label = structure(1L, .Label = "d", class = "factor"), 
                             estimator_label = "estimator", term = "group2", bias = NA_real_, 
                             `se(bias)` = NA_real_, rmse = NA_real_, `se(rmse)` = NA_real_, 
                             power = 0, `se(power)` = 0, coverage = NA_real_, `se(coverage)` = NA_real_, 
                             mean_estimate = 1.58, `se(mean_estimate)` = 0, sd_estimate = 0, 
                             `se(sd_estimate)` = 0, mean_se = 0.849091017238762, `se(mean_se)` = 0, 
                             type_s_rate = NaN, `se(type_s_rate)` = NA_real_, mean_estimand = NA_real_, 
                             `se(mean_estimand)` = NA_real_, n_sims = 4L), class = "data.frame", row.names = c(NA, 
                                                                                                               -1L))
  
expect_equivalent(diagnose_design(d, sims = 4, bootstrap_sims = 5)$diagnosands_df, sleep_ols)
  
})

test_that("diagnosis, NAs if no estimand", {
  mu <- declare_estimand(mean(extra))
  d <- declare_population(sleep) + mu
  
  sleep_ols <-
    structure(list(design_label = structure(1L, .Label = "d", class = "factor"), 
                   estimand_label = "estimand", bias = NA_real_, `se(bias)` = NA_real_, 
                   rmse = NA_real_, `se(rmse)` = NA_real_, power = NA_real_, 
                   `se(power)` = NA_real_, coverage = NA_real_, `se(coverage)` = NA_real_, 
                   mean_estimate = NA_real_, `se(mean_estimate)` = NA_real_, 
                   sd_estimate = NA_real_, `se(sd_estimate)` = NA_real_, mean_se = NA_real_, 
                   `se(mean_se)` = NA_real_, type_s_rate = NA_real_, `se(type_s_rate)` = NA_real_, 
                   mean_estimand = 1.54, `se(mean_estimand)` = 0, n_sims = 4L), class = "data.frame", row.names = c(NA, 
                                                                                                                    -1L))
  
    expect_equivalent(diagnose_design(d, sims = 4)$diagnosands_df, sleep_ols)
})

test_that("error if diagnosand not named", {
  expect_error(declare_diagnosands(mean(foo)), "All diagnosands must be named")
})


test_that("subset diagnosands", {

  # add a diagnosand
  my_diags <- declare_diagnosands(perc_above_p05 = mean(p.value > .05), subset = p.value < .05)

  dx <- diagnose_design(my_design, diagnosands = my_diags, sims = 4, bootstrap_sims = FALSE)
  expect_equal(dx$diagnosands_df$perc_above_p05, 0)
})

test_that("declare time errors", {
  expect_s3_class(declare_diagnosands(bias = mean(estimate - estimand)), "design_step")
  expect_error(declare_diagnosands(), "No diagnosands were declared.")
  expect_s3_class(declare_diagnosands(my_diag = mean(p.value)), "design_step")
})


# test_that("missingness",{
#   
#   my_population <- declare_population(N = 50, noise = rnorm(N))
#   fixed_pop <-  my_population()
#   my_pop <- declare_population(fixed_pop)
# 
#   my_odd_estimator <- function(data) {
#     estimate = rnorm(1)
#     if(estimate > 0){
#       estimate <- NA
#     }
#     data.frame(estimate = estimate)
#   }
#   # my_odd_estimator(my_pop)
#   estimator <- declare_estimator(handler = my_odd_estimator)
#   des <- my_pop + estimator
#   
#   dx <- diagnose_design(des, sims = 50, bootstrap_sims = FALSE)
#   expect_equal(
#     names(dx$diagnosands_df),
#     c(
#       "design_label",
#       "bias",
#       "rmse",
#       "power",
#       "coverage",
#       "mean_estimate",
#       "sd_estimate",
#       "mean_se",
#       "type_s_rate",
#       "mean_estimand",
#       "n_deleted",
#       "n_sims"
#     )
#   )
#   
#   diags <- declare_diagnosands(select = c(mean_estimate), na.rm = TRUE)
#   dx <- diagnose_design(des, sims = 50, diagnosands = diags, bootstrap_sims = FALSE)
#   expect_equal(names(dx$diagnosands_df), c("design_label", "mean_estimate", "n_deleted", "n_sims"))
#   
#   diags <- declare_diagnosands(select = c(mean_estimate), na.rm = FALSE)
#   dx <- diagnose_design(des, sims = 50, diagnosands = diags, bootstrap_sims = FALSE)
#   expect_equal(names(dx$diagnosands_df), c("design_label", "mean_estimate", "n_sims"))
#   
#   
# })

