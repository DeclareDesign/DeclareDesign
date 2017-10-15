context("model estimator")

test_that("test the estimators", {
  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_potential_outcomes <-
    declare_potential_outcomes(Y_Z_0 = draw_binary(noise, link = "probit"),
                               Y_Z_1 = draw_binary(noise + 2, link = "probit"))
  my_assignment <- declare_assignment()
  my_design <- declare_design(my_population,
                              my_potential_outcomes,
                              my_assignment,
                              reveal_outcomes)
  dat <- draw_data(my_design)

  # lm
  estimator_lm <-
    declare_estimator(Y ~ Z, model = lm, coefficient_name = "Z")
  estimator_lm_nocoef <- declare_estimator(Y ~ Z, model = lm)

  estimator_lm(dat)
  estimator_lm_nocoef(dat)

  estimator_lm <-
    declare_estimator(Y ~ Z,
                      model = lm,
                      coefficient_name = "Z",
                      label = "my_lm")
  estimator_lm_nocoef <-
    declare_estimator(Y ~ Z, model = lm, label = "my_lm")

  estimator_lm(dat)
  estimator_lm_nocoef(dat)

  estimator_lm_robust <-
    declare_estimator(Y ~ Z,
                      model = lm_robust,
                      coefficient_name = "Z")
  debugonce(estimator_lm_robust)
  estimator_lm_robust(dat)



  # glm
  estimator_glm <-
    declare_estimator(Y ~ Z, model = glm, coefficient_name = "Z")
  estimator_glm_nocoef <- declare_estimator(Y ~ Z, model = glm)

  estimator_glm(dat)
  estimator_glm_nocoef(dat)

  estimator_glm <-
    declare_estimator(Y ~ Z,
                      model = glm,
                      coefficient_name = "Z",
                      label = "my_glm")
  estimator_glm_nocoef <-
    declare_estimator(Y ~ Z, model = glm, label = "my_glm")

  estimator_glm(dat)
  estimator_glm_nocoef(dat)

  # logit
  estimator_logit <-
    declare_estimator(Y ~ Z,
                      model = glm,
                      family = binomial,
                      coefficient_name = "Z")
  estimator_logit_nocoef <-
    declare_estimator(Y ~ Z, model = glm, family = binomial)

  estimator_logit(dat)
  estimator_logit_nocoef(dat)

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

  estimator_logit(dat)
  estimator_logit_nocoef(dat)

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

  estimator_probit(dat)
  estimator_probit_nocoef(dat)

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
