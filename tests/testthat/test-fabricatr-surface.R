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

# The rest of the draw_*() family ----
#
# Each of these was run against 1.1.1 with fabricatr 1.0.2 first and agrees
# with it draw for draw, except where fabricatr's own NEWS says 2.0 differs
# on purpose. What is asserted here is what the *design* sees: the function
# is on the search path because fabricatr is a `Depends`, its arguments may
# be declared parameters, it reads columns drawn earlier in the same step,
# and the column it returns is one a later step can use.

test_that("draw_binomial() takes its trial count from a declared parameter", {
  design <-
    declare_parameters(trials = 4) +
    declare_model(N = 500, D = draw_binomial(N = N, prob = 0.3, trials = trials))

  set.seed(343)
  dat <- draw_data(design)
  expect_true(all(dat$D %in% 0:4))
  expect_lt(abs(mean(dat$D) - 1.2), 0.15)

  set.seed(343)
  wider <- draw_data(redesign(design, trials = 8))
  expect_true(all(wider$D %in% 0:8))
  expect_lt(abs(mean(wider$D) - 2.4), 0.25)
})

test_that("draw_count() takes its mean from a declared parameter", {
  design <-
    declare_parameters(lambda = 2) +
    declare_model(N = 500, D = draw_count(N = N, mean = lambda))

  set.seed(343)
  dat <- draw_data(design)
  expect_true(all(dat$D >= 0) && all(dat$D == floor(dat$D)))
  expect_lt(abs(mean(dat$D) - 2), 0.25)

  set.seed(343)
  expect_lt(abs(mean(draw_data(redesign(design, lambda = 6))$D) - 6), 0.5)
})

test_that("draw_categorical() reads a per-row probability matrix built in the step", {
  # The matrix is assembled out of a column drawn a moment earlier in the
  # same declaration, which is the whole reason it has to be tested here:
  # fabricatr sees the assembled matrix and never the column.
  set.seed(343)
  dat <- draw_data(declare_model(N = 500, X = runif(N),
                                 D = draw_categorical(prob = cbind(X, 1 - X))))
  expect_true(all(dat$D %in% 1:2))
  # Category 1 is drawn with probability X, so it falls on the larger X.
  expect_gt(mean(dat$X[dat$D == 1]), mean(dat$X[dat$D == 2]))
})

test_that("draw_categorical() labels its categories", {
  set.seed(343)
  dat <- draw_data(declare_model(
    N = 400, D = draw_categorical(N = N, prob = c(0.2, 0.3, 0.5),
                                  labels = c("a", "b", "c"))))
  expect_s3_class(dat$D, "factor")
  expect_equal(levels(dat$D), c("a", "b", "c"))
  expect_gt(sum(dat$D == "c"), sum(dat$D == "a"))
})

test_that("draw_likert() cuts a latent column into bins", {
  # A value outside [min, max] takes the outermost bin rather than NA, and the
  # codes are integer. Both are deliberate fabricatr 2.0 changes, recorded in
  # its NEWS; 1.0.2 returned doubles and two NAs on this draw.
  set.seed(343)
  dat <- draw_data(declare_model(N = 400, X = rnorm(N),
                                 D = draw_likert(x = X, min = -3, max = 3, bins = 5)))
  expect_true(all(dat$D %in% 1:5))
  expect_false(anyNA(dat$D))
  expect_gt(cor(dat$X, dat$D), 0.9)
})

test_that("draw_ordered() cuts at interior break points", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 400, X = rnorm(N),
                                 D = draw_ordered(x = X, breaks = c(-1, 0, 1))))
  expect_equal(sort(unique(dat$D)), 1:4)
  expect_equal(max(dat$X[dat$D == 1]), max(dat$X[dat$X < -1]))
})

test_that("draw_quantile() and split_quantile() make equal-sized groups", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 400, D = draw_quantile(N = N, type = 4),
                                 X = rnorm(N), Q = split_quantile(x = X, type = 4)))
  expect_equal(as.vector(table(dat$D)), rep(100L, 4))
  expect_equal(as.vector(table(dat$Q)), rep(100L, 4))
  # split_quantile() cuts the column it is given; draw_quantile() ignores it.
  expect_lt(max(dat$X[dat$Q == 1]), min(dat$X[dat$Q == 2]))
})

test_that("an ICC draw reads the cluster column and the declared ICC", {
  design <-
    declare_parameters(icc = 0.8) +
    declare_model(N = 500, cl = rep(1:50, each = 10),
                  U = draw_normal_icc(mean = 0, clusters = cl, ICC = icc, sd = 1),
                  D = draw_binary_icc(prob = 0.5, clusters = cl, ICC = icc))

  set.seed(343)
  dat <- draw_data(design)
  expect_true(all(dat$D %in% 0:1))
  spread <- function(d) var(tapply(d$U, d$cl, mean))

  set.seed(343)
  independent <- draw_data(redesign(design, icc = 0))
  # Clustered draws move together, so the cluster means are further apart.
  expect_gt(spread(dat), 3 * spread(independent))
})

test_that("draw_multivariate() makes several columns from one declaration", {
  # One named dot produces two columns, which is a shape nothing else in a
  # declaration does, and the step has to carry both out.
  set.seed(343)
  dat <- draw_data(declare_model(N = 1000, draw_multivariate(
    c(X1, X2) ~ MASS::mvrnorm(n = N, mu = c(0, 0),
                              Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)))))
  expect_equal(names(dat), c("ID", "X1", "X2"))
  expect_lt(abs(cor(dat$X1, dat$X2) - 0.5), 0.1)
})

test_that("correlate() draws a variable against a column drawn before it", {
  set.seed(343)
  dat <- draw_data(declare_model(N = 2000, X = rnorm(N),
                                 D = correlate(draw_binary, given = X,
                                               rho = 0.7, prob = 0.5)))
  expect_true(all(dat$D %in% 0:1))
  expect_lt(abs(mean(dat$D) - 0.5), 0.05)
  expect_gt(mean(dat$X[dat$D == 1]) - mean(dat$X[dat$D == 0]), 0.5)
})

test_that("recycle() repeats a short vector over the step's rows", {
  dat <- draw_data(declare_model(N = 6, D = recycle(c(1, 2, 3))))
  expect_equal(dat$D, c(1, 2, 3, 1, 2, 3))
})

test_that("a clustered design draws, assigns and estimates by cluster", {
  # The end of the path: an ICC draw supplies the correlated outcome,
  # randomizr assigns whole clusters, and estimatr clusters the standard
  # error. The three packages are `Depends` and meet only inside a design.
  design <-
    declare_parameters(icc = 0.5, ate = 0.4) +
    declare_model(N = 600, cl = rep(1:60, each = 10),
                  U = draw_normal_icc(mean = 0, clusters = cl, ICC = icc, sd = 1),
                  Y_Z_0 = U, Y_Z_1 = U + ate) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = cluster_ra(clusters = cl)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, clusters = cl, inquiry = "ATE")

  set.seed(343)
  out <- run_design(design)
  expect_equal(out$estimand, 0.4)
  expect_lt(abs(out$estimate - 0.4), 0.5)
  # Treatment is constant within a cluster, which is what makes it a cluster
  # design rather than a design with a cluster column.
  dat <- draw_data(design)
  expect_true(all(tapply(dat$Z, dat$cl, function(z) length(unique(z))) == 1))
})

# The level functions ----
#
# `add_level()` and `nest_level()` are reached elsewhere in this suite. These
# are the rest of the hierarchy vocabulary, which a design reaches only
# through `declare_model()`, and whose results later steps then have to be
# able to sample, assign and estimate over.

test_that("declare_level() and cross_levels() build the crossing of two levels", {
  set.seed(343)
  dat <- draw_data(declare_model(
    region = add_level(N = 4, u_region = rnorm(N)),
    year = declare_level(N = 3, u_year = rnorm(N)),
    obs = cross_levels(.by = join_using(region, year), Y = u_region + u_year)))

  expect_equal(nrow(dat), 12L)
  expect_equal(names(dat),
               c("region", "u_region", "year", "u_year", "obs", "Y"))
  # Every region appears in every year, which is what crossing means.
  expect_equal(as.vector(table(dat$region)), rep(3L, 4))
  expect_equal(as.vector(table(dat$year)), rep(4L, 3))
  expect_length(unique(dat$u_region), 4L)
})

test_that("link_levels() matches two levels, and rho correlates the match", {
  design <-
    declare_parameters(rho = 0.9) +
    declare_model(
      worker = add_level(N = 100, u_worker = rnorm(N)),
      firm = declare_level(N = 100, u_firm = rnorm(N)),
      job = link_levels(N = 400, .by = join_using(worker, firm), rho = rho,
                        Y = u_worker + u_firm))

  set.seed(343)
  dat <- draw_data(design)
  expect_equal(nrow(dat), 400L)
  # `rho` correlates which unit is matched to which, by level id, and not the
  # variables drawn on the two levels: those stay independent at any rho.
  expect_gt(cor(as.numeric(dat$worker), as.numeric(dat$firm)), 0.8)
  expect_lt(abs(cor(dat$u_worker, dat$u_firm)), 0.2)

  set.seed(343)
  independent <- draw_data(redesign(design, rho = 0))
  expect_lt(abs(cor(as.numeric(independent$worker),
                    as.numeric(independent$firm))), 0.2)
})

test_that("modify_level() writes a group summary back onto every row", {
  # `.by` takes the *name* of a grouping column, not the column.
  set.seed(343)
  dat <- draw_data(
    declare_model(cluster = add_level(N = 10, u = rnorm(N)),
                  unit = add_level(N = 5, Y = rnorm(N, u)),
                  unit = modify_level(cluster_mean = mean(Y), .by = "cluster")) +
      declare_measurement(Y_centred = Y - cluster_mean))

  expect_equal(nrow(dat), 50L)
  expect_length(unique(dat$cluster_mean), 10L)
  expect_lt(abs(mean(dat$Y_centred)), 1e-12)
})

test_that("import_level() takes a real table as the top level, and the design carries it", {
  # A design is a value: the frame the declaration read is carried by the
  # design, so it still draws once the environment that built it is gone.
  build <- function() {
    districts <- data.frame(district = letters[1:4], n_voters = c(10, 20, 30, 40))
    declare_model(district = import_level(data = districts),
                  voter = add_level(N = 3, Y = rnorm(N, n_voters)))
  }
  set.seed(343)
  dat <- draw_data(build())
  expect_equal(nrow(dat), 12L)
  expect_equal(sort(unique(dat$district)), letters[1:4])
  expect_gt(mean(dat$Y[dat$district == "d"]), mean(dat$Y[dat$district == "a"]))
})

test_that("resample_data() bootstraps clusters with ALL", {
  # ALL is fabricatr's own sentinel for "as many as there are", and a
  # cluster bootstrap is the design that wants it.
  set.seed(343)
  base <- fabricate(cluster = add_level(N = 10, u = rnorm(N)),
                    unit = add_level(N = 5, Y = rnorm(N, u)))
  design <- declare_model(
    data = base,
    handler = function(data) {
      resample_data(data, N = c(ALL, 3), ID_labels = c("cluster", "unit"))
    })

  dat <- draw_data(design)
  expect_equal(nrow(dat), 30L)
  expect_lte(length(unique(dat$cluster)), 10L)
  # Two draws of the same design are different resamples.
  expect_false(identical(draw_data(design)$Y, draw_data(design)$Y))
})
