

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
  skip_if_not_installed(c("AER", "broom"))
  des <- pop + declare_estimator(Y ~ D | Z, model = AER::ivreg)
  expect_equal(ncol(get_estimates(des)), 9)
})

test_that("lm", {
  des <- pop + declare_estimator(Y ~ Z, model = lm)
  expect_equal(ncol(get_estimates(des)), 9)
})

test_that("glm", {
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "probit"))
  expect_equal(ncol(get_estimates(des)), 9)
  des <- pop + declare_estimator(D ~ Z, model = glm, family = binomial(link = "logit"))
  expect_equal(ncol(get_estimates(des)), 9)
})


test_that("betareg", {
  skip_if_not_installed(c("betareg", "broom"))
  des <- pop + declare_estimator(D2 ~ Z, model = betareg::betareg)
  expect_equal(ncol(get_estimates(des)), 9)
})


test_that("biglm", {
  skip_if_not_installed(c("biglm", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = biglm::biglm)
  expect_equal(ncol(get_estimates(des)), 7)
})

test_that("gam", {
  skip_if_not_installed(c("gam", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = gam::gam)
  expect_equal(ncol(get_estimates(des)), 7)
})

test_that("lfe", {
  skip_if_not_installed(c("lfe", "broom"))
  des <- pop + declare_estimator(Y ~ Z, model = lfe::felm)
  expect_equal(ncol(get_estimates(des)), 8)
})


test_that("polr", {
  skip_if_not_installed(c("polr", "broom"))
  des <- pop + declare_estimator(Y_fac ~ Z, model = MASS::polr)
  suppressWarnings(expect_error(get_estimates(des)))
})
  