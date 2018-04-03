context("Diagnosands")

my_population <- declare_population(N = 50, noise = rnorm(N))

my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = noise,
                             Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_assignment <- declare_assignment(m = 25)

pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")

pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")

reveal_outcomes <- declare_reveal()


my_design <- declare_design(my_population(),
                            my_potential_outcomes, pate,
                            my_assignment,
                            reveal_outcomes,
                            pate_estimator)


test_that("parallel works.", {
  #TODO use future

  skip_if_not_installed("future.apply")
  skip_on_cran()

  suppressWarnings(
    diag <- diagnose_design(my_design, sims = 2, bootstrap = FALSE)
  )

  expect_output(print(diag), regexp = "Research design diagnosis")

})

test_that("Diagnosis prints ok", {


  diag <- diagnose_design(my_design, sims = 2, bootstrap = FALSE)

  ## diagnose_design(my_design, sims = 2, bootstrap = FALSE, parallel = TRUE)

  expect_output(print(diag), regexp = "Research design diagnosis")

})


test_that("test diagnosands without estimands", {

  my_design2 <- declare_design(my_population,
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes,
                              no_estimand=declare_estimator(Y~Z))

  my_dig <-  declare_diagnosands(mean_est = mean(est), sd_est = sd(est))
  diagnosis <- diagnose_design(my_design2, sims = 2, diagnosands = my_dig, bootstrap = FALSE)

  head(diagnosis$simulations)

  expect_equal(dim(diagnosis$diagnosands), c(1,4))

})


test_that("custom diagnosand function", {

  mean_custom <- function(x) return(mean(x * 5))

  my_dig <-  declare_diagnosands(mean_x5 = mean_custom(est), mean_true = mean(est))

  rm(mean_custom)
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = FALSE)

  expect_true("mean_x5" %in% names(diagnosis$diagnosands))


  # works with two with bootstrapping
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_dig, bootstrap = 2)

  expect_true("se(mean_x5)" %in% names(diagnosis$diagnosands))
})


test_that("single diagnosand function", {


  # works with only one diagnosand with bootstrapping (!)
  my_one_dig <-  declare_diagnosands(se_bias = mean(se - sd(estimand)))
  diagnosis <- diagnose_design(my_design, sims = 2, diagnosands = my_one_dig)

  expect_true("se_bias" %in% names(diagnosis$diagnosands))
})


test_that("no estimates, no estimators should error", {
  my_population <- declare_population(N = 50)
  my_design <- declare_design(my_population)
  head(draw_data(my_design))

  expect_error(diagnose_design(my_design, sims = 2, bootstrap = FALSE))

})





test_that("diagnosis, list of designs",{

  d <- declare_design(sleep, declare_estimator(extra~group, coefficients=group2))

  diagnosand <- declare_diagnosands(z=mean(est> 0))

  expect_error(diagnose_design(sleep), "Please only send design objects to diagnose_design")

  diag1 <- diagnose_design(list(d,d), diagnosands = diagnosand, sims = 5)
  diag2 <- diagnose_design(design_1=d,design_2=d, diagnosands = diagnosand, sims = 5)

  expect_identical(diag1, diag2)

})

test_that("diagnosis, unlinked estimator", {
  d <- declare_design(sleep, declare_estimand(foo=2, bar=3), declare_estimator(extra~group, model=lm, coefficients=TRUE))

  expect_warning( diagnose_design(d, sims = 5), "Estimators lack estimand/coefficient labels for matching, a many-to-many merge was performed.")
})


test_that("diagnosis, no estimator", {
  d <- declare_design(sleep, declare_estimand(foo=2, bar=3))

  diagnosand <- declare_diagnosands(z=mean(estimand > 0))

  expect_identical( diagnose_design(d, diagnosands = diagnosand, sims = 5)$diagnosand,
                    structure(list(estimand_label = c("bar", "foo"), z = c(1, 1),
                                   `se(z)` = c(0, 0)), .Names = c("estimand_label", "z", "se(z)"
                                   ), class = "data.frame", row.names = c("bar", "foo"))
                    )
})


test_that("Overriding join conditions",{
  skip_if_not_installed("reshape2")
  skip_if_not_installed("dplyr")

  require(dplyr)
  require(reshape2)

  alpha <- .05

  custom <- declare_diagnosands(handler = function(data) {
                                  data %>% group_by(sim_ID) %>%
                                  summarize(any_significant = any(p < alpha),
                                            num_significant = sum(p < alpha),
                                            all_significant = all(p < alpha)) %>%
                                  summarize(any_significant = mean(any_significant),
                                            num_significant = mean(num_significant),
                                            all_significant = mean(all_significant)) %>%
                                    melt(id.vars=NULL, variable.name="estimand_label", value.name="estimand")
  })

  attr(custom, "group_by") <- c("estimand_label", "estimator_label")

  design <- declare_population(sleep, handler=fabricatr::resample_data) /
            declare_estimand(group1=1, group2=2, coefficients=TRUE, label="e") /
            declare_estimator(extra~group+0, coefficients=TRUE, estimand="e", model=lm, label="my_estimator")

  diagnosands <- get_diagnosands(diagnose_design(design, diagnosands = custom))

  expect_true(is.data.frame(diagnosands) && nrow(diagnosands) == 1)

})

test_that("diagnosis, NAs if no estimand", {
  d <- declare_design(sleep, ols = declare_estimator(extra~group))

golden <-
  structure(list(estimator_label = "ols", coefficient = "group2",
                 bias = NA_real_, `se(bias)` = NA_real_, rmse = NA_real_,
                 `se(rmse)` = NA_real_, power = 0, `se(power)` = 0, coverage = NA_real_,
                 `se(coverage)` = NA_real_, mean_estimate = 1.58, `se(mean_estimate)` = 0,
                 sd_estimate = 0, `se(sd_estimate)` = 0, type_s_rate = NaN,
                 `se(type_s_rate)` = NA_real_, mean_estimand = NA_real_, `se(mean_estimand)` = NA_real_), .Names = c("estimator_label",
                 "coefficient", "bias", "se(bias)", "rmse", "se(rmse)", "power",
                 "se(power)", "coverage", "se(coverage)", "mean_estimate", "se(mean_estimate)",
                 "sd_estimate", "se(sd_estimate)", "type_s_rate", "se(type_s_rate)",
                 "mean_estimand", "se(mean_estimand)"), class = "data.frame", row.names = "ols.group2")


  expect_identical( diagnose_design(d, sims=4)$diagnosands, golden)

})

test_that("diagnosis, NAs if no estimand", {
  d <- declare_design(sleep, mu = declare_estimand(mean(extra)))

  golden <-
    structure(list(estimand_label = "mu", bias = NA_real_, `se(bias)` = NA_real_,
                   rmse = NA_real_, `se(rmse)` = NA_real_, power = NA_real_,
                   `se(power)` = NA_real_, coverage = NA_real_, `se(coverage)` = NA_real_,
                   mean_estimate = NA_real_, `se(mean_estimate)` = NA_real_,
                   sd_estimate = NA_real_, `se(sd_estimate)` = NA_real_, type_s_rate = NA_real_,
                   `se(type_s_rate)` = NA_real_, mean_estimand = 1.54, `se(mean_estimand)` = 0), .Names = c("estimand_label",
                      "bias", "se(bias)", "rmse", "se(rmse)", "power", "se(power)",
                      "coverage", "se(coverage)", "mean_estimate", "se(mean_estimate)",
                      "sd_estimate", "se(sd_estimate)", "type_s_rate", "se(type_s_rate)",
                      "mean_estimand", "se(mean_estimand)"), class = "data.frame", row.names = "mu")
  expect_identical( diagnose_design(d, sims=4)$diagnosands, golden)

})


test_that("diagnosis, sorted by estimator order in design", {
  d <- declare_design(sleep,
                      declare_estimand(m=mean(extra)),
                      declare_estimator(extra~group, label="X4", estimand="m"),
                      declare_estimator(extra~group, label="X3", estimand="m"),
                      declare_estimator(extra~group, label="X2", estimand="m"),
                      declare_estimator(extra~group, label="X1", estimand="m"))

  dx <- diagnose_design(d)

  expect_true(!is.unsorted(rev(dx$diagnosands$estimator_label)))
})



test_that("error if diagnosand not named", {
  expect_error(declare_diagnosands(mean(foo)), "All diagnosands must be named")
})

