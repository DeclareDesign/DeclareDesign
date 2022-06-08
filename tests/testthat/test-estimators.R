context("Estimators")

my_population <- declare_model(N = 500, noise = rnorm(N))
my_potential_outcomes <- declare_model(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

expect_estimates <- function(estimates, label = NULL) {
  expect_equal(
    names(estimates),
    c("estimator", "term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "df", "outcome")
  )
  if (is.character(label)) {
    expect_equal(estimates$estimator, label)
  }
}

test_that("difference in means", {
  my_estimator <- declare_estimator(Y ~ Z)
  my_population() %>% my_potential_outcomes() %>% my_assignment() %>% my_measurement() %>% my_estimator() %>% expect_estimates()
})

test_that("lm with robust ses", {
  my_estimator <- declare_estimator(Y ~ Z, .method = lm_robust)
  my_population() %>% my_potential_outcomes() %>% my_assignment() %>% my_measurement() %>% my_estimator() %>% expect_estimates()
})


test_that("lm with HC3 robust ses", {
  my_estimator <- declare_estimator(Y ~ Z, .method = lm_robust, se_type = "HC3")
  my_population() %>% my_potential_outcomes() %>% my_assignment() %>% my_measurement() %>% my_estimator() %>% expect_estimates()
})

test_that("custom estimator function", {
  my_mean <- function(data) {
    data.frame(estimate = with(data, 2), foo = mean(data$Y))
  }
  my_estimator_custom <- declare_estimator(handler = label_estimator(my_mean))
  cust <- my_population() %>% my_potential_outcomes() %>% my_assignment() %>% my_measurement() %>% my_estimator_custom()
  expect_equal(cust$estimate, 2)
})

test_that("check blocked d-i-m estimator", {
  my_population <- declare_model(N = 500, noise = rnorm(N), blocks = sample(rep(c("A", "B"), each = 250), N, replace = F))
  my_potential_outcomes <- declare_model(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2) + 5 * (blocks == "A"))
  my_assignment <- declare_assignment(Z = block_ra(blocks = blocks))

  ## lm with HC3 robust ses
  my_estimator_blocked <- declare_estimator(Y ~ Z, .method = difference_in_means, blocks = `blocks`)
  df <- my_population() %>% my_potential_outcomes() %>% my_assignment() %>% my_measurement()
  my_estimator_notblocked <- declare_estimator(Y ~ Z)

  df %>% my_estimator_notblocked() %>% expect_estimates()
  df %>% my_estimator_blocked() %>% expect_estimates() ## it is different!
})



test_that("regression from estimatr works as an estimator", {
  my_population <- declare_model(N = 500, noise = rnorm(N))
  my_potential_outcomes <- declare_model(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 100))
  pate <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "pate")
  pate_estimator <- declare_estimator(Y ~ Z + noise,
    .method = lm_robust,
    term = "noise",
    inquiry = pate, label = "pate_hat"
  )
  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 

  my_design <- my_population +
    my_potential_outcomes +
    pate +
    my_assignment +
    my_measurement +
    pate_estimator

  estimate <- draw_estimates(my_design)
  expect_equal(estimate$estimator, "pate_hat")
  expect_equal(estimate$term, "noise")
  expect_equal(estimate$inquiry, "pate")
})

population <- declare_model(N = 200, noise = rnorm(N))

potential_outcomes <- declare_potential_outcomes(formula = Y ~ noise + Z * .5)

pop <- potential_outcomes(population())

sampling <- declare_sampling(S = complete_rs(N, n = 100))

assignment <- declare_assignment(Z = complete_ra(N, m = 50))

sate <- declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0))

my_reveal_outcomes <- declare_reveal()

test_that("multiple estimator declarations work", {
  estimator_1 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate,
      label = "estimator_1"
    )

  estimator_2 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate,
      label = "estimator_2"
    )

  design <- population +
    potential_outcomes +
    sampling +
    sate +
    assignment +
    my_reveal_outcomes +
    estimator_1 +
    estimator_2

  e <- draw_estimates(design)

  expect_equal(e$estimator, c("estimator_1", "estimator_2"))
})

test_that("multiple estimator declarations work", {
  estimator_3 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate,
      label = "estimator_3"
    )

  estimator_4 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate,
      label = "estimator_4"
    )

  design <- population +
    potential_outcomes +
    sampling +
    sate +
    assignment +
    my_reveal_outcomes +
    estimator_3 +
    estimator_4

  e <- draw_estimates(design)

  expect_equal(e$estimator, c("estimator_3", "estimator_4"))
})

test_that("multiple estimator declarations work", {
  estimator_5 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate
    )

  estimator_6 <-
    declare_estimator(
      formula = Y ~ Z,
      .method = estimatr::lm_robust,
      inquiry = sate
    )

  # This could eventually be fixed so that the estimator names are inherited
  expect_error({
    design <- population +
      potential_outcomes +
      sampling +
      sate +
      assignment +
      my_reveal_outcomes +
      estimator_5 +
      estimator_6
  })
})

context("Labeling estimator output with inquiries")

inquiry_arg_label <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
inquiry_explicit_label <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "ATE")

expect_label <- function(df, expected_label, inquiry) {
  expect_equal(df$estimator, expected_label)
  expect_equal(df$inquiry, inquiry)
}

df <- data.frame(
  Y = c(0, 0, 0, 0, 1, 1, 1, 1),
  Z = c(0, 0, 0, 0, 1, 1, 1, 1)
)

test_that("labels for estimates and inquiries work inquiry splat labeld estimator default", {
  mator_no_label <- declare_estimator(Y ~ Z, inquiry = inquiry_arg_label)
  df %>% mator_no_label() %>% expect_label("estimator", "ATE")
})

test_that("labels for estimates and inquiries work, label explicit, inquiry splat labeled", {
  mator_label <- declare_estimator(Y ~ Z, inquiry = inquiry_arg_label, label = "an_estimator")
  df %>% mator_label() %>% expect_label("an_estimator", "ATE")
})

test_that("labels for estimates and inquiries work inquiry splat labeld label =NULL", {
  mator_label_null <- declare_estimator(Y ~ Z, inquiry = inquiry_arg_label, label = NULL)
  expect_error(df %>% mator_label_null())
})

test_that("labels for estimates and inquiries work - label default", {
  mator_no_label <- declare_estimator(Y ~ Z, inquiry = inquiry_explicit_label)
  df %>% mator_no_label() %>% expect_label("estimator", "ATE")
})

test_that("labels for estimates and inquiries work - label explicit", {
  mator_label <- declare_estimator(Y ~ Z, inquiry = inquiry_explicit_label, label = "an_estimator")
  # mator_label_noquote <- declare_estimator(Y ~ Z, inquiry = inquiry_explicit_label, label = an_estimator)
  df %>% mator_label() %>% expect_label("an_estimator", "ATE")
})

test_that("labels for estimates and inquiries work- label=NULL", {
  mator_label_null <- declare_estimator(Y ~ Z, inquiry = inquiry_explicit_label, label = NULL)
  expect_error(df %>% mator_label_null())
})


test_that("labels for estimates and inquiries work inquiry label, estimator default", {
  mator_no_label <- declare_estimator(Y ~ Z, inquiry = inquiry_explicit_label)
  df %>% mator_no_label() %>% expect_label("estimator", "ATE")
})


test_that("coefficient_names = TRUE returns all term", {
  tst <-
    data.frame(
      x = runif(100),
      y = runif(100),
      wt = runif(100),
      clust = sample(1:10, 100, replace = TRUE)
    )

  est4 <- declare_estimator(
    y ~ x + as.factor(clust),
    clusters = clust,
    weights = wt,
    .method = lm_robust,
    term = TRUE
  )

  result <- est4(tst)

  expect_gt(nrow(result), 0)
})


test_that("default estimator handler validation fn", {
  expect_error(declare_estimator(.method = "Test"))
  expect_error(declare_estimator(.method = I))
})

test_that("label_estimator, handler does not take data", {
  expect_error(label_estimator(I), "function with a data argument")
})

test_that("method_handler runs directly", {
  lm_out <- structure(list(term = "group2", estimate = 1.58, std.error = 0.849091017238762, 
                           statistic = 1.86081346748685, p.value = 0.0791867142159382, 
                           conf.low = -0.203874032287599, conf.high = 3.3638740322876), row.names = c(NA, 
                                                                                                      -1L), class = c("tbl_df", "tbl", "data.frame"))

  result <- method_handler(sleep, extra ~ group, .method = lm, term = "group2")
  expect_equivalent(as.data.frame(result), as.data.frame(lm_out))
})


test_that("estimators have different columns", {
  skip_if_not_installed("Matching")
  population <- declare_model(
    N = 1000,
    X1 = rnorm(N),
    X2 = rnorm(N),
    X3 = rnorm(N)
  )

  potential_outcomes <-
    declare_potential_outcomes(formula = Y ~ X1 + X2 + X3 + Z)

  assignment <- declare_assignment(
    handler = function(data) {
      prob <- with(data, pnorm(X1 + X2 + X3))
      data$Z <- rbinom(nrow(data), 1, prob)
      return(data)
    }
  )

  att <- declare_inquiry(att = mean(Y_Z_1[Z == 1] - Y_Z_0[Z == 1]))

  estimator_d_i_m <- declare_estimator(Y ~ Z, inquiry = "inquiry", label = "dim")

  matching_helper <- function(data) {
    match_out <- with(data, Matching::Match(
      Y = Y,
      Tr = Z,
      X = cbind(X1, X2, X3)
    ))
    return(data.frame(estimate = match_out$est))
  }

  estimator_m <- declare_estimator(
    handler = label_estimator(matching_helper),
    inquiry = att,
    label = "matching"
  )

  matching <- population +
    potential_outcomes +
    assignment +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    estimator_d_i_m +
    estimator_m

  result <- draw_estimates(matching)

  expect_length(result, 11)
  expect_equal(nrow(result), 2)
})


test_that("when a term is missing from a model there is an informative error", {
  data <- fabricate(
    N = 100,
    Y = rbinom(N, 1, .5),
    Z = rbinom(N, 1, .5)
  )
  ols <- declare_estimator(Y ~ Z, .method = lm_robust, term = "X")

  expect_error(ols(data), "Not all of the terms declared in your estimator are present in the estimator's output, including X.")
})

test_that("estimators and estimands are in the correct order when specified", {
  
  design <- 
    declare_model(
      N = 20,
      X1 = rnorm(N),
      X2 = rnorm(N),
      Y = X1 - X2 + rnorm(N)
    ) +
    declare_inquiry(
      x1 = 1, 
      x2 = -1, 
      interaction = 0) +
    declare_estimator(Y ~ X1*X2, .method = lm_robust, 
                      term = c("X1:X2", "X1", "X2"),
                      inquiry = c("interaction", "x1", "x2"))
  
  ret <- run_design(design)    
  expect_equal(ret$inquiry, c("interaction", "x1", "x2"))
  expect_equal(ret$term, c("X1:X2", "X1", "X2"))
})
