# What a user writes inside a declaration, and fabricatr has to answer.
#
# `test-fabricatr-contract.R` asserts the private entry point this package
# calls. This file asserts the other half of the boundary: the fabricatr
# functions a *user* writes inside `declare_model()` and the other verbs.
# fabricatr is a `Depends`, so all 27 of its exports sit on the search path of
# every design written here, and DeclareDesign's own suite reaches 9 of them.
#
# fabricatr's tests call these functions directly, with ordinary arguments,
# outside any design. Inside a declaration none of that holds: the arguments
# arrive as quosures spliced through `fabricate_with_dots()`, the row count
# comes from the step rather than from the call, a column drawn earlier in the
# same step is already in the data mask, and the value may be bound by
# `declare_parameters()` and then changed by `redesign()`. A test on
# fabricatr's side cannot reach any of it.
#
# Every number asserted here was first measured against DeclareDesign 1.1.1
# with fabricatr 1.0.2 in `~/git_projects/.cran_reference_lib`, and agrees
# with it: what these tests pin is the behaviour the released package already
# has, not a choice made by the rewrite.

# draw_binary() ----

test_that("draw_binary() reads a declared parameter, and redesign() changes it", {
  design <-
    declare_parameters(p = 0.2) +
    declare_model(N = 2000, D = draw_binary(N = N, prob = p))

  set.seed(343)
  dat <- draw_data(design)
  expect_equal(nrow(dat), 2000L)
  expect_equal(sort(unique(dat$D)), c(0, 1))
  expect_lt(abs(mean(dat$D) - 0.2), 0.03)

  set.seed(343)
  expect_lt(abs(mean(draw_data(redesign(design, p = 0.8))$D) - 0.8), 0.03)
})

test_that("a sweep over the probability gives one design per value", {
  design <-
    declare_parameters(p = 0.2) +
    declare_model(N = 2000, D = draw_binary(N = N, prob = p))

  set.seed(343)
  designs <- redesign(design, p = c(0.1, 0.9))
  expect_length(designs, 2L)
  means <- vapply(designs, function(d) mean(draw_data(d)$D), numeric(1))
  expect_lt(abs(means[[1]] - 0.1), 0.03)
  expect_lt(abs(means[[2]] - 0.9), 0.03)
})

test_that("prob without N draws one value and recycles it over the rows", {
  # `draw_binary()` defaults `N = length(prob)`, so a scalar `prob` and no `N`
  # is one Bernoulli draw, which fabricate() then recycles to every row. The
  # column is constant and the design does not complain. 1.1.1 does the same,
  # and it is why the book always spells `N = N`.
  set.seed(343)
  dat <- draw_data(declare_model(N = 50, D = draw_binary(prob = 0.2)))
  expect_equal(nrow(dat), 50L)
  expect_length(unique(dat$D), 1L)
})

test_that("prob reads a column drawn earlier in the same step", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 2000, X = runif(N), D = draw_binary(prob = X)))
  expect_equal(nrow(dat), 2000L)
  expect_lt(abs(mean(dat$D) - 0.5), 0.03)
  # The draw follows the column it was given rather than ignoring it.
  expect_gt(mean(dat$X[dat$D == 1]), mean(dat$X[dat$D == 0]))
})

test_that("latent = with a link draws on the probability scale", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 2000, X = rnorm(N),
                                 D = draw_binary(latent = X, link = "logit")))
  expect_equal(sort(unique(dat$D)), c(0, 1))
  expect_lt(abs(mean(dat$D) - 0.5), 0.03)
})

test_that("a fabricatr error inside a declaration names the step that raised it", {
  # The two ways a draw_binary() call goes wrong, reached through a verb
  # rather than called directly: a probability outside [0, 1], and a `link`
  # given alongside `prob`, which acts on `latent` and so has nothing to do.
  # Both must arrive wrapped in the step, because a user reading the message
  # has a design in hand and not a call.
  out_of_range <- rlang::catch_cnd(
    draw_data(declare_model(N = 10, D = draw_binary(N = N, prob = 1.4))))
  expect_match(conditionMessage(out_of_range), "declare_model")
  expect_match(conditionMessage(out_of_range), "prob")

  link_on_prob <- rlang::catch_cnd(
    draw_data(declare_model(N = 10, X = rnorm(N),
                            D = draw_binary(prob = X, link = "logit"))))
  expect_match(conditionMessage(link_on_prob), "declare_model")
  expect_match(conditionMessage(link_on_prob), "link")
})

test_that("draw_binary() runs in a measurement step without adding an id", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 2000, X = runif(N)) +
                     declare_measurement(D = draw_binary(prob = X)))
  expect_equal(names(dat), c("ID", "X", "D"))
  expect_lt(abs(mean(dat$D) - 0.5), 0.03)
})

test_that("a design assigning treatment with draw_binary() diagnoses and redesigns", {
  # The whole path, because the surface is only worth anything if a design
  # built on it runs: a parameter sets the assignment probability, the
  # estimator is a difference in means, and the redesign changes only the
  # probability.
  design <-
    declare_parameters(p = 0.5, ate = 0.3) +
    declare_model(N = 500, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + ate) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = draw_binary(N = N, prob = p)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, inquiry = "ATE")

  set.seed(343)
  out <- run_design(design)
  expect_equal(out$estimand, 0.3)
  expect_lt(abs(out$estimate - 0.3), 0.25)

  set.seed(343)
  lopsided <- draw_data(redesign(design, p = 0.1))
  expect_lt(abs(mean(lopsided$Z) - 0.1), 0.05)
})
