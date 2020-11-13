context("model estimator")
library(DeclareDesign)
library(testthat)

my_population <- declare_population(N = 500, noise = rnorm(N))
my_potential_outcomes <-
  declare_potential_outcomes(
    Y_Z_0 = draw_binary(latent = noise, link = "probit"),
    Y_Z_1 = draw_binary(latent = noise + 2, link = "probit")
  )
my_assignment <- declare_assignment()
my_reveal <- reveal_outcomes()
my_design <- my_population +
  my_potential_outcomes +
  my_assignment +
  my_reveal
dat <- draw_data(my_design)

test_that("test default term Z, lm", {
  # lm
  estimator_lm <-
    declare_estimator(Y ~ Z, model = lm, term = "Z")
  estimator_lm_nocoef <- declare_estimator(Y ~ Z, model = lm)

  expect_equal(
    estimator_lm(dat),
    estimator_lm_nocoef(dat)
  )

  estimator_lm_robust <-
    declare_estimator(Y ~ Z,
      model = lm_robust,
      term = "Z"
    )
  expect_equivalent(
    estimator_lm(dat),
    estimator_lm_robust(dat)[, 1:8]
  )
})

test_that("test estimators, labels, quoted Z", {
  estimator_lm <-
    declare_estimator(Y ~ Z,
      model = lm,
      term = "Z",
      label = "my_lm"
    )
  estimator_lm_nocoef <-
    declare_estimator(Y ~ Z, model = lm, label = "my_lm")

  expect_identical(
    estimator_lm(dat),
    estimator_lm_nocoef(dat)
  )
})

test_that("test GLM estimators, default vs explicit Z", {
  estimator_glm <-
    declare_estimator(Y ~ Z, model = glm, term = "Z")

  estimator_glm_nocoef <- declare_estimator(Y ~ Z, model = glm)

  expect_identical(
    estimator_glm(dat),
    estimator_glm_nocoef(dat)
  )
})

test_that("test GLM estimators with label", {
  estimator_glm <-
    declare_estimator(Y ~ Z,
      model = glm,
      term = "Z",
      label = "my_glm"
    )
  estimator_glm_nocoef <-
    declare_estimator(Y ~ Z, model = glm, label = "my_glm")

  expect_identical(
    estimator_glm(dat),
    estimator_glm_nocoef(dat)
  )
})

test_that("test logit default vs explicit Z", {

  # logit
  estimator_logit <-
    declare_estimator(Y ~ Z,
      model = glm,
      family = binomial,
      term = "Z"
    )
  estimator_logit_nocoef <-
    declare_estimator(Y ~ Z, model = glm, family = binomial)

  expect_identical(
    estimator_logit(dat),
    estimator_logit_nocoef(dat)
  )

  estimator_logit <-
    declare_estimator(
      Y ~ Z,
      model = glm,
      family = binomial,
      term = "Z",
      label = "my_logit"
    )
  estimator_logit_nocoef <-
    declare_estimator(Y ~ Z,
      model = glm,
      family = binomial,
      label = "my_logit"
    )

  expect_identical(
    estimator_logit(dat),
    estimator_logit_nocoef(dat)
  )

  # probit
  estimator_probit <-
    declare_estimator(
      Y ~ Z,
      model = glm,
      family = binomial(link = "probit"),
      term = "Z"
    )
  estimator_probit_nocoef <-
    declare_estimator(Y ~ Z, model = glm, family = binomial(link = "probit"))

  expect_identical(
    estimator_probit(dat),
    estimator_probit_nocoef(dat)
  )

  estimator_probit <-
    declare_estimator(
      Y ~ Z,
      model = glm,
      family = binomial(link = "probit"),
      term = "Z",
      label = "my_probit"
    )
  estimator_probit_nocoef <-
    declare_estimator(
      Y ~ Z,
      model = glm,
      family = binomial(link = "probit"),
      label = "my_probit"
    )

  estimator_probit(dat)
  estimator_probit_nocoef(dat)
})





dat <-
  data.frame(
    Y = rep(1:5, 20),
    Y_fac = factor(rep(1:5, 20)),
    Z = rep(c(0, 1), c(50, 50)),
    D = rep(c(0, 1, 0, 1), c(20, 30, 10, 40)),
    D2 = rep(c(0.1, .9, 0.1, .9), c(20, 30, 10, 40))
  )


pop <- declare_population(dat)



test_that("custom tidy method", {
  model_function <- function(data){
    return(structure(list(est = 1), class = "my_modelr"))
  }
  
  des <- pop + declare_estimator(model = model_function)
  
  expect_error(draw_estimates(des), "We were unable to tidy the output")

  tidy.my_modelr <- function(fit, conf.int = TRUE){
    return(data.frame(term = "my-term", est = 1, stringsAsFactors = TRUE))
  }
  
  # this is an ugly hack per https://github.com/r-lib/testthat/issues/720
  assign("tidy.my_modelr", tidy.my_modelr, envir = .GlobalEnv)
  
  des <- pop + declare_estimator(model = model_function)
  
  expect_equal(draw_estimates(des), structure(list(estimator_label = "estimator", term = structure(1L, .Label = "my-term", class = "factor"), 
                                                   est = 1), row.names = c(NA, -1L), class = "data.frame"))
  
})

library(broom)

test_that("AER", {
  skip_if_not_installed(c("AER", "broom"))
  library(broom)
  des <- pop + declare_estimator(Y ~ D | Z, model = AER::ivreg)
  expect_equal(ncol(draw_estimates(des)), 8)
})

test_that("lm", {
  des <- pop + declare_estimator(Y ~ Z, model = lm)
  expect_equal(ncol(draw_estimates(des)), 8)
})

test_that("glm", {
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "probit"))
  expect_equal(ncol(draw_estimates(des)), 8)
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "logit"))
  expect_equal(ncol(draw_estimates(des)), 8)
})


test_that("betareg", {
  skip_if_not_installed(c("betareg", "broom"))
  des <- pop + declare_estimator(D2 ~ Z, model = betareg::betareg)
  if(packageVersion("broom") <= "0.5.0") {
    expect_error(draw_estimates(des))
  } else {
    expect_equal(ncol(draw_estimates(des)), 9)
  }
})

test_that("biglm", {
  skip_if_not_installed(c("biglm", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = biglm::biglm)
  if(packageVersion("broom") <= "0.5.0") {
    expect_error(draw_estimates(des))
  } else {
    expect_equal(ncol(draw_estimates(des)), 7)
  }
})

test_that("gam", {
  skip_if_not_installed(c("gam", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = gam::gam)
  if(packageVersion("broom") <= "0.5.0") {
    expect_error(draw_estimates(des))
  } else if(packageVersion("gam") < "1.16") {
    expect_warning(expect_equal(ncol(draw_estimates(des)), 7))
  } else {
    expect_equal(ncol(draw_estimates(des)), 7)
  }
  
  
})

test_that("lfe", {
  skip_if_not_installed(c("lfe", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = lfe::felm)
  expect_equal(ncol(draw_estimates(des)), 8)
})


test_that("polr", {
  skip_if_not_installed(c("MASS", "broom"))
  des <- pop + declare_estimator(Y_fac ~ Z, model = MASS::polr)
  suppressWarnings(expect_error(draw_estimates(des)))
})



