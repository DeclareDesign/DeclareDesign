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

test_that("redesign with .expand = FALSE zips parameters", {
  designer <- function(N, ate) {
    declare_model(N = N, Y = rnorm(N) + ate) +
      declare_inquiry(mu = ate)
  }
  design <- designer(N = 50, ate = 0.5)
  fam <- redesign(design, N = c(25, 75), ate = c(0.1, 0.2), .expand = FALSE)
  expect_length(fam, 2L)
})

test_that("expand_design builds a list from a designer", {
  designer <- function(N) declare_model(N = N, Y = rnorm(N))
  fam <- suppressWarnings(expand_design(designer, N = c(10, 20)))
  expect_length(fam, 2L)
})

test_that("expand_design accepts function-valued parameters in zip mode", {
  designer <- function(N = 100, fn = mean) {
    declare_model(N = N, Y = rnorm(N)) +
      declare_inquiry(inq = fn(Y))
  }
  fam <- suppressWarnings(expand_design(designer, N = c(10, 50),
                       fn = c(mean, median), .expand = FALSE))
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

test_that("a bare list handed to a list-valued parameter warns", {
  levels_list <- list(party = c("Left", "Right"), region = c("North", "South"))
  d <- declare_model(N = 20, U = rnorm(N)) +
    declare_inquiry(k = length(levels_list))

  # The elements are character vectors rather than lists, so this is one value
  # split into two designs, which is the mistake the warning exists for.
  expect_warning(
    redesign(d, levels_list = list(party = c("A", "B"), region = c("N", "S"))),
    "currently holds a list"
  )
  # Wrapping is the documented escape, and a list of lists is a real sweep.
  expect_no_warning(redesign(d, levels_list = list(levels_list)))
  fam <- expect_no_warning(
    redesign(d, levels_list = list(levels_list, levels_list["party"]))
  )
  expect_length(fam, 2L)
  expect_equal(draw_estimands(fam[[1]])$estimand, 2)
  expect_equal(draw_estimands(fam[[2]])$estimand, 1)
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

  # a literal argument is not a parameter, and says so rather than quietly
  # keeping the value the design was written with
  literal <- declare_model(N = 30, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_error(redesign(literal, N = 50), "not a parameter")
})

test_that("the refusal leads with the ordinary route and quotes the design back", {
  literal <- declare_model(N = 30, Y = rnorm(N)) +
    declare_inquiry(target = 0.25)
  msg <- conditionMessage(tryCatch(redesign(literal, N = 50),
                                   error = function(e) e))
  # The value the argument holds, and the verb it sits in, so the advice can be
  # pasted rather than translated.
  expect_match(msg, "`N <- 30`", fixed = TRUE)
  expect_match(msg, "declare_model(N = N, ...)", fixed = TRUE)
  # A name outside the design comes first; declare_parameters() is the second
  # suggestion, because most designs do not need it.
  expect_lt(regexpr("name outside the design", msg, fixed = TRUE),
            regexpr("declare_parameters", msg, fixed = TRUE))

  # the verb reported is the step the argument is actually in
  msg2 <- conditionMessage(tryCatch(redesign(literal, target = 0.5),
                                    error = function(e) e))
  expect_match(msg2, "declare_inquiry(target = target, ...)", fixed = TRUE)
  expect_match(msg2, "`target <- 0.25`", fixed = TRUE)
})

test_that("a parameter named d is reachable, and so are de, des, desi, desig", {
  # Macartan: "`.design` is a good solution for d arguments; this has tripped
  # me up before." `mediation_analysis` in the library has a parameter named
  # `d`, and an undotted first formal partially matched it, so the design
  # object was replaced by the number and `redesign()` failed on its own type
  # check. Every prefix of `design` had the same problem.
  local({
    for (nm in c("d", "de", "des", "desi", "desig", "design")) {
      env <- new.env()
      assign(nm, 2, envir = env)
      quo <- rlang::new_quosure(rlang::sym(nm), env = env)
      step <- eval(rlang::expr(declare_model(N = !!rlang::sym(nm), Y = rnorm(N))),
                   envir = env)
      out <- do.call(redesign, c(list(step), setNames(list(5), nm)))
      expect_equal(nrow(draw_data(out)), 5L, info = nm)
    }
  })
  # and the same for expand_design()'s first formal
  designer <- function(d = 2) declare_model(N = d, Y = rnorm(N))
  fam <- suppressWarnings(expand_design(designer, d = c(3, 6)))
  expect_equal(vapply(fam, function(x) nrow(draw_data(x)), integer(1)),
               c(design_1 = 3L, design_2 = 6L))
})

test_that("a column does not make its own parameter unreachable", {
  # Macartan's `a` case. The design reads `a` from outside for the row count
  # and creates a column called `a` in the same step. A redesign must move the
  # first and leave the second where the declaration put it.
  local({
    a <- 4
    design <- declare_model(N = a, a = 5) + NULL
    expect_equal(nrow(draw_data(design)), 4L)
    expect_equal(unique(draw_data(design)$a), 5)
    moved <- draw_data(redesign(design, a = 3))
    expect_equal(nrow(moved), 3L)
    expect_equal(unique(moved$a), 5)
  })
})

test_that("a name the design expects redesign to supply is not refused", {
  # The designer form, written at top level: nothing binds `N` or `prob`
  # anywhere, and `redesign()` is what supplies them. Refusing these would
  # break every design written to be called through a designer function.
  declaration <- declare_model(N = N, U = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N = N, prob = prob))
  skip_if_not_installed("randomizr")
  designs <- redesign(declaration, N = c(20, 40), prob = 0.5)
  expect_length(designs, 2L)
  expect_equal(vapply(designs, function(d) nrow(draw_data(d)), integer(1)),
               c(design_1 = 20L, design_2 = 40L))
  # and they are still not reported as parameters, because they hold nothing
  expect_false("N" %in% design_parameters(declaration)$name)
})

test_that("N is the rows being built in every step, not only the one that declares it", {
  # `declare_model(N = m) + declare_model(U = rnorm(N))`: the second step's `N`
  # is fabricate's row count, so a workspace `N` of the same name is not a
  # parameter of this design and redesigning it says so.
  local({
    N <- 2
    m <- 1
    design <- declare_model(N = m) + declare_model(U = rnorm(N))
    expect_equal(design_parameters(design)$name, "m")
    expect_error(redesign(design, N = 2), "not a parameter")
    expect_equal(nrow(draw_data(redesign(design, m = 5))), 5L)
  })
})

test_that("both routes the refusal names actually work", {
  N <- 30
  outside <- declare_model(N = N, Y = rnorm(N))
  expect_equal(nrow(draw_data(redesign(outside, N = 50))), 50L)

  declared <- declare_parameters(N = 30) + declare_model(N = N, Y = rnorm(N))
  expect_equal(nrow(draw_data(redesign(declared, N = 50))), 50L)

  designer <- function(N = 30) declare_model(N = N, Y = rnorm(N))
  expect_equal(nrow(draw_data(redesign(designer(), N = 50))), 50L)
})

test_that("a declared parameter is reached by name and a column of that name is not", {
  # The branch that used to replace an argument because its *name* matched is
  # gone from every step but `declare_parameters()`. It is what made
  # `redesign(sd = 3)` write 3 into a column called `sd`, and what made
  # `diff_in_diff` put the character "Y" where its outcome belonged.
  design <- declare_parameters(sd = 2) +
    declare_model(N = 100, u = rnorm(N), sd = sd^2)
  expect_equal(unique(draw_data(design)$sd), 4)
  expect_equal(unique(draw_data(redesign(design, sd = 3))$sd), 9)
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
  expect_output(print(summary(design, run = FALSE)), "Parameters and objects")
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
  design <- declare_parameters(n = 20) +
    declare_model(N = n, Y = rnorm(N)) +
    declare_assignment(Z = randomizr::complete_ra(N))
  expect_no_warning(redesign(design, n = 40))
  expect_equal(nrow(draw_data(redesign(design, n = 40))), 40L)
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
  # The data are reached by the name the design reads them under, `small`,
  # rather than by the name of fabricate's argument. `data` names the argument
  # and belongs to the declaration; `small` names the object a redesign reaches.
  design <- declare_model(data = small, Y = Y_star + 1) + NULL
  expect_equal(nrow(draw_data(design)), 30L)
  expect_true("small" %in% design_parameters(design)$name)
  expect_error(redesign(design, data = big), "not a parameter")

  swapped <- redesign(design, small = big)
  expect_s3_class(swapped, "design")
  expect_equal(nrow(draw_data(swapped)), 121L)
  expect_no_warning(redesign(design, small = big))

  # wrapping still works, and a list of two data frames is still two designs
  expect_equal(nrow(draw_data(redesign(design, small = list(big)))), 121L)
  fam <- redesign(design, small = list(small, big))
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

test_that("a custom step's arguments stay as written through a redesign", {
  local({
    k <- 2
    design <- declare_model(N = 6, pair = rep(1:3, each = 2),
                            role = rep(c("A", "B"), 3), a = 1:6, kk = k) +
      declare_step(id_cols = pair, names_from = role, values_from = c(a),
                   handler = tidyr::pivot_wider)
    # `id_cols = pair` must reach pivot_wider() as the name `pair`, not as the
    # column's contents, on the rebuilt step as well as the declared one.
    expect_equal(names(draw_data(design)), c("pair", "A", "B"))
    expect_equal(names(draw_data(redesign(design, k = 3))), c("pair", "A", "B"))
    expect_equal(unique(draw_data(design)$kk), NULL)
  })
})

test_that("expand_design() is deprecated and redesign() of the designer's result replaces it", {
  designer <- function(N = 50) declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  rlang::reset_warning_verbosity("expand_design")
  expect_warning(old <- expand_design(designer, N = c(10, 20)), "deprecated")
  new <- redesign(designer(), N = c(10, 20))
  expect_equal(length(new), 2L)
  expect_equal(nrow(draw_data(new[[2]])), 20L)
  expect_equal(nrow(draw_data(old[[2]])), 20L)
})
