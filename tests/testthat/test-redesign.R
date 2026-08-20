test_that("redesign returns a single design when one combination is supplied", {
  design <- simple_design(N = 30)
  d2 <- redesign(design, N = 100)
  expect_s3_class(d2, "design")
  df <- draw_data(d2)
  expect_equal(nrow(df), 100L)
})

test_that("redesign expands the parameter grid by default", {
  design <- simple_design(N = 30)
  fam <- redesign(design, N = c(20, 40))
  expect_length(fam, 2L)
  ns <- vapply(fam, function(d) nrow(draw_data(d)), integer(1))
  expect_equal(unname(sort(ns)), c(20L, 40L))
})

test_that("redesign with expand = FALSE zips parameters", {
  designer <- function(N, ate) {
    declare_model(N = N, Y = rnorm(N) + ate) +
      declare_inquiry(mu = ate)
  }
  design <- designer(N = 50, ate = 0.5)
  fam <- redesign(design, N = c(25, 75), ate = c(0.1, 0.2), expand = FALSE)
  expect_length(fam, 2L)
})

test_that("expand_design builds a list from a designer", {
  designer <- function(N) declare_model(N = N, Y = rnorm(N))
  fam <- expand_design(designer, N = c(10, 20))
  expect_length(fam, 2L)
})

test_that("expand_design accepts function-valued parameters in zip mode", {
  designer <- function(N = 100, fn = mean) {
    declare_model(N = N, Y = rnorm(N)) +
      declare_inquiry(inq = fn(Y))
  }
  fam <- expand_design(designer, N = c(10, 50),
                       fn = c(mean, median), expand = FALSE)
  expect_length(fam, 2L)
  expect_equal(nrow(draw_data(fam[[1]])), 10L)
  expect_equal(nrow(draw_data(fam[[2]])), 50L)
})

test_that("redesign accepts list-valued parameters", {
  prob_each <- c(.2, .5, .3)
  d <- declare_model(N = 300, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = prob_each))
  fam <- redesign(d, prob_each = list(c(.2, .5, .3), c(0, .5, .5)))
  expect_length(fam, 2L)
  expect_equal(nrow(draw_data(fam[[1]])), 300L)
  # The new probabilities really are in force: arm 1 is never assigned.
  expect_equal(unname(table(draw_data(fam[[2]])$Z)["T1"]), 0L)
})

test_that("a bare vector handed to a vector-valued parameter warns", {
  prob_each <- c(.2, .5, .3)
  d <- declare_model(N = 300, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = prob_each))
  expect_warning(redesign(d, prob_each = c(0, .5, .5)),
                 "currently holds 3 values")
  expect_no_warning(redesign(d, prob_each = list(c(0, .5, .5))))

  # sweeping a scalar parameter is the ordinary case and stays quiet
  N <- 30
  scalar <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(scalar, N = c(50, 100)))
})

test_that("a parameter written inline inside a call is not redesignable", {
  # `prob_each` here is the name of an argument to complete_ra(), not a name
  # the design reads out of an environment, so nothing can rebind it. The
  # warning is the whole point: silently returning an unchanged design is how
  # this used to look.
  d <- declare_model(N = 30, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N, num_arms = 3,
                                                   prob_each = c(.2, .5, .3)))
  expect_warning(redesign(d, prob_each = list(c(0, .5, .5))),
                 "not found in the design")
})

test_that("redesign warns about a parameter no step responds to", {
  design <- simple_design(N = 30)
  expect_warning(redesign(design, b = 2), "b is not found in the design")
  expect_warning(redesign(design, b = 2, cc = 3), "are not found in the design")
})

test_that("redesign is silent about parameters it does change", {
  N <- 30
  design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(design, N = 50))

  # a literal argument is redesignable too, and must not warn
  literal <- declare_model(N = 30, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_no_warning(redesign(literal, N = 50))
  expect_equal(nrow(draw_data(redesign(literal, N = 50))), 50L)
})

test_that("redesign replaces a function-valued parameter", {
  g <- function(x) mean(x)
  design <- declare_model(N = 40, Y = c(rep(0, 39), 100)) +
    declare_inquiry(inq = g(Y))
  expect_equal(draw_estimands(design)$estimand, 2.5)

  swapped <- redesign(design, g = stats::median)
  expect_s3_class(swapped, "design")
  expect_equal(draw_estimands(swapped)$estimand, 0)

  # varying a function needs a list, one element per design
  fam <- redesign(design, g = list(mean, stats::median))
  expect_length(fam, 2L)
  expect_equal(draw_estimands(fam[[2]])$estimand, 0)
})

test_that("a function-valued parameter reaches the simulations table as its source", {
  g <- function(x) mean(x)
  design <- declare_model(N = 20, Y = rnorm(N)) +
    declare_inquiry(inq = g(Y)) +
    declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                      inquiry = "inq", label = "ols")
  sims <- simulate_design(redesign(design, g = stats::median), sims = 3)
  expect_true("g" %in% names(sims))
  expect_type(sims$g, "character")
})

test_that("summary lists the parameters and objects the design refers to", {
  N <- 200
  g <- function(x) mean(x)
  design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(inq = g(Y))
  objects <- find_all_objects(design)
  expect_true(all(c("N", "g") %in% objects$name))
  # a package function is not a parameter of the design
  expect_false("rnorm" %in% objects$name)
  # neither is a column an earlier step created
  expect_false("Y" %in% objects$name)
  expect_output(summary(design), "Parameters and objects")
})

test_that("the redesign warning is not silenced by a package of the same name", {
  # Regression test from Macartan's crash course. `env_has_var()` inherited all
  # the way to base, so `redesign(design, n = 200)` on a design with no `n`
  # found `dplyr::n` and stayed quiet. Any short parameter name an attached
  # package exports had the same effect.
  local({
    N <- 100
    design <- declare_model(N = N, Y = rnorm(N)) + declare_inquiry(Q = 0)
    expect_true(exists("n", envir = as.environment("package:dplyr")))
    expect_warning(redesign(design, n = 200), "n is not found in the design")
    expect_no_warning(redesign(design, N = 200))
  })
})

test_that("a design that reads a package object is still redesignable", {
  skip_if_not_installed("randomizr")
  design <- declare_model(N = 20, Y = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N))
  expect_no_warning(redesign(design, N = 40))
})

test_that("a missing argument in a subscript is not treated as a name", {
  # From ResearchDesigns' latent_variables and multilevel. The blank in
  # `scores[, 1]` parses to a symbol whose name is "", and asking an
  # environment about the empty name errors, so the whole parameter list
  # failed with "attempt to use zero-length variable name".
  local({
    N <- 40
    scores <- matrix(rnorm(2 * N), ncol = 2)
    design <- declare_model(N = N, Y = scores[, 1]) + declare_inquiry(Q = mean(Y))
    objects <- find_all_objects(design)
    expect_true(all(c("N", "scores") %in% objects$name))
    expect_false("" %in% objects$name)
    expect_no_warning(redesign(design, N = 20))
  })
})

test_that("a data frame is one replacement value and needs no wrapping", {
  # `make_design(id, data = df)` in ResearchDesigns errored here: a data frame
  # is a list, so the grid builder asked for one design per column.
  small <- fabricate(N = 30, Y_star = rnorm(N))
  big <- fabricate(N = 121, Y_star = rnorm(N))
  design <- declare_model(data = small, Y = Y_star + 1) + NULL
  expect_equal(nrow(draw_data(design)), 30L)

  swapped <- redesign(design, data = big)
  expect_s3_class(swapped, "design")
  expect_equal(nrow(draw_data(swapped)), 121L)
  expect_no_warning(redesign(design, data = big))

  # wrapping still works, and a list of two data frames is still two designs
  expect_equal(nrow(draw_data(redesign(design, data = list(big)))), 121L)
  fam <- redesign(design, data = list(small, big))
  expect_length(fam, 2L)
  expect_equal(vapply(fam, function(d) nrow(draw_data(d)), integer(1)),
               c(design_1 = 30L, design_2 = 121L))
})

test_that("a matrix-valued parameter is replaced rather than swept", {
  local({
    weights <- matrix(1, nrow = 2, ncol = 2)
    design <- declare_model(N = 2, Y = as.numeric(weights %*% c(1, 1))) + NULL
    expect_equal(draw_data(design)$Y, c(2, 2))

    swapped <- redesign(design, weights = matrix(3, nrow = 2, ncol = 2))
    expect_s3_class(swapped, "design")
    expect_equal(draw_data(swapped)$Y, c(6, 6))
    expect_no_warning(redesign(design, weights = matrix(3, nrow = 2, ncol = 2)))
  })
})

test_that("an estimator's term and inquiry follow a redesign", {
  # multi_arm_designer(m_arms = 4) assigned four arms, declared three contrasts
  # and reported two estimates. `term` and `inquiry` were ordinary arguments,
  # evaluated when the estimator was written, so no redesign could reach them.
  local({
    m_arms <- 3
    design <- declare_model(N = 300, u = rnorm(N)) +
      declare_assignment(Z = sample(rep(seq_len(m_arms), length.out = 300))) +
      declare_inquiry(
        handler = function(data, m_arms) {
          ks <- seq_len(m_arms)[-1]
          data.frame(inquiry = paste0("ate_", ks), estimand = ks - 1)
        },
        m_arms = m_arms
      ) +
      declare_measurement(Y = u + as.numeric(Z)) +
      declare_estimator(Y ~ factor(Z), .method = lm,
                        term = paste0("factor(Z)", seq_len(m_arms)[-1]),
                        inquiry = paste0("ate_", seq_len(m_arms)[-1]))

    expect_equal(draw_estimates(design)$term, c("factor(Z)2", "factor(Z)3"))

    wider <- redesign(design, m_arms = 4)
    estimates <- draw_estimates(wider)
    expect_equal(estimates$term, c("factor(Z)2", "factor(Z)3", "factor(Z)4"))
    expect_equal(estimates$inquiry, c("ate_2", "ate_3", "ate_4"))
    expect_equal(nrow(draw_estimands(wider)), 3L)

    # the design that was redesigned from is untouched
    expect_equal(draw_estimates(design)$term, c("factor(Z)2", "factor(Z)3"))
  })
})

test_that("declare_test's term follows a redesign", {
  local({
    keep <- "Z"
    design <- declare_model(N = 100, Z = rep(0:1, 50), W = rnorm(N),
                            Y = Z + W + rnorm(N)) +
      declare_test(Y ~ Z + W, .method = lm, term = keep)
    expect_equal(draw_estimates(design)$term, "Z")
    expect_equal(draw_estimates(redesign(design, keep = "W"))$term, "W")
  })
})

test_that("an inquiry passed as a step object still fails where it is written", {
  step <- declare_inquiry(ATE = 0)
  expect_error(
    declare_estimator(Y ~ Z, .method = lm, inquiry = step),
    "label as a string"
  )
})
