context("model estimator")

my_population <- declare_population(N = 500, noise = rnorm(N))
my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = draw_binary(latent=noise, link = "probit"),
                             Y_Z_1 = draw_binary(latent=noise + 2, link = "probit"))
my_assignment <- declare_assignment()
my_reveal <- declare_reveal()
my_design <- declare_design(my_population,
                            my_potential_outcomes,
                            my_assignment,
                            my_reveal)
dat <- draw_data(my_design)


test_that("test default coefficient Z, lm", {

  # lm
  estimator_lm <-
    declare_estimator(Y ~ Z, model = lm, coefficient_name = "Z")
  estimator_lm_nocoef <- declare_estimator(Y ~ Z, model = lm)

  expect_equal(
    estimator_lm(dat),
    estimator_lm_nocoef(dat)
  )

  estimator_lm_robust <-
    declare_estimator(Y ~ Z,
                      model = lm_robust,
                      coefficient_name = "Z")
  expect_equal(
    estimator_lm(dat),
    estimator_lm_robust(dat)
  )

})

test_that("test estimators, labels, quoted Z", {

  estimator_lm <-
    declare_estimator(Y ~ Z,
                      model = lm,
                      coefficient_name = "Z",
                      label = "my_lm")
  estimator_lm_nocoef <-
    declare_estimator(Y ~ Z, model = lm, label = "my_lm")

  expect_identical(
    estimator_lm(dat),
    estimator_lm_nocoef(dat)
  )
})

test_that("test GLM estimators, default vs explicit Z", {
  estimator_glm <-
    declare_estimator(Y ~ Z, model = glm, coefficient_name = "Z")

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
                      coefficient_name = "Z",
                      label = "my_glm")
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
                      coefficient_name = "Z")
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
      coefficient_name = "Z",
      label = "my_logit"
    )
  estimator_logit_nocoef <-
    declare_estimator(Y ~ Z,
                      model = glm,
                      family = binomial,
                      label = "my_logit")

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
      coefficient_name = "Z"
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
      coefficient_name = "Z",
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
