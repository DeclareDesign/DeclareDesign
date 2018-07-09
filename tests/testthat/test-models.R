

dat <-
  data.frame(
    Y = rep(1:5, 20),
    Y_fac = factor(rep(1:5, 20)),
    Z = rep(c(0, 1), c(50, 50)),
    D = rep(c(0, 1, 0, 1), c(20, 30, 10, 40)),
    D2 = rep(c(0.1, .9, 0.1, .9), c(20, 30, 10, 40))
  )


pop <- declare_population(dat)


test_that("AER", {
  skip_if_not_installed("AER")
  des <- pop + declare_estimator(Y ~ D | Z, model = AER::ivreg)
  expect_equal(ncol(get_estimates(des)), 7)
})

test_that("lm", {
  des <- pop + declare_estimator(Y ~ Z, model = lm)
  expect_equal(ncol(get_estimates(des)), 7)
})

test_that("glm", {
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "probit"))
  expect_equal(ncol(get_estimates(des)), 7)
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "logit"))
  expect_equal(ncol(get_estimates(des)), 7)
})


test_that("betareg", {
  # fails
  skip_if_not_installed("betareg")
  des <- pop + declare_estimator(D2 ~ Z, model = betareg::betareg)
  expect_error(ncol(get_estimates(des)))
  
  fit <- betareg::betareg(D2 ~ Z, data = dat)
  summary(fit)
  confint(fit)
  
})


test_that("biglm", {
  # Fails
  des <- pop + declare_estimator(Y ~ Z, model = biglm::biglm)
  expect_error(ncol(get_estimates(des)))

  fit <- biglm::biglm(Y ~ Z, data = dat)
  summary(fit)
})

test_that("gam", {
  # Fails
  des <- pop + declare_estimator(Y ~ Z, model = gam::gam)
  expect_error(ncol(get_estimates(des)))
  
  fit <- gam::gam(Y ~ Z, data = dat)
  summary(fit)
})

test_that("lfe", {
  des <- pop + declare_estimator(Y ~ Z, model = lfe::felm)
  expect_equal(ncol(get_estimates(des)), 7)
})


test_that("polr", {
  # Fails
  des <- pop + declare_estimator(Y_fac ~ Z, model = MASS::polr)
  expect_error(ncol(get_estimates(des)))
  
  fit <- MASS::polr(Y_fac ~ Z, data = dat)
  summary(fit)
  confint(fit)
})