test_that("a name a data step declares is a column, not a parameter", {
  local({
    Y <- 999
    design <- declare_model(N = 50, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
    objects <- find_all_objects(design)
    expect_false("Y" %in% objects$name)
    # `N = 50` is a number the design writes down, not a name it reads from
    # anywhere, so there is nothing for a redesign to change.
    expect_false("N" %in% objects$name)
  })
})

test_that("an argument written as a literal cannot be redesigned", {
  local({
    design <- declare_model(N = 50, Y = rnorm(N)) +
      declare_inquiry(target = 0.5)
    objects <- find_all_objects(design)
    expect_equal(nrow(objects), 0L)
    expect_error(redesign(design, N = 80), "not a parameter")
    expect_error(redesign(design, target = 1), "not a parameter")
  })
})

test_that("the same numbers can be redesigned once declared or named outside", {
  local({
    declared <- declare_parameters(n = 50) +
      declare_model(N = n, Y = rnorm(N))
    expect_true("n" %in% design_parameters(declared)$name)
    expect_equal(nrow(draw_data(redesign(declared, n = 80))), 80L)

    n_units <- 50
    outside <- declare_model(N = n_units, Y = rnorm(N))
    expect_true("n_units" %in% design_parameters(outside)$name)
    expect_equal(nrow(draw_data(redesign(outside, n_units = 80))), 80L)
  })
})

test_that("a name passed to a handler stays visible to every later step", {
  # From multi_arm_designer(m_arms = 4), which assigned four arms, declared
  # three contrasts and estimated two. Every named dot used to be masked for
  # the rest of the design, so once `declare_inquiry(handler = f, m_arms = ...)`
  # had used the name, the estimator's `term` was invisible to the finder and
  # `redesign()` could report a parameter as absent that the design does read.
  local({
    m_arms <- 3
    design <- declare_model(N = 60, u = rnorm(N)) +
      declare_inquiry(
        handler = function(data, m_arms) {
          ks <- seq_len(m_arms)[-1]
          data.frame(inquiry = paste0("ate_", ks), estimand = 0)
        },
        m_arms = m_arms
      ) +
      declare_assignment(Z = sample(rep(seq_len(m_arms), length.out = 60))) +
      declare_measurement(Y = u + as.numeric(Z)) +
      declare_estimator(Y ~ factor(Z), .method = lm,
                        term = paste0("factor(Z)", seq_len(m_arms)[-1]),
                        inquiry = paste0("ate_", seq_len(m_arms)[-1]))
    steps <- find_all_objects(design)$step[find_all_objects(design)$name == "m_arms"]
    expect_true(all(c(2L, 5L) %in% steps))
  })
})

test_that("N keeps meaning the rows being built after a redesign moves it", {
  local({
    design <- declare_parameters(n = 50) +
      declare_model(N = n, Y = rnorm(N)) +
      declare_measurement(W = rnorm(N))
    bigger <- redesign(design, n = 80)
    df <- draw_data(bigger)
    expect_equal(nrow(df), 80L)
    # `rnorm(N)` in the measurement step reads the rows it has, not the 50 the
    # design was written with and not the 80 the parameter now holds.
    expect_equal(sum(!is.na(df$W)), 80L)
  })
})

test_that("N read outside a data step is an ordinary parameter", {
  local({
    n_arms <- 3
    design <- declare_model(N = 50, Y = rnorm(N),
                            Z = sample(rep(seq_len(n_arms), length.out = 50))) +
      declare_estimator(Y ~ factor(Z), .method = lm,
                        term = paste0("factor(Z)", seq_len(n_arms)[-1]))
    objects <- find_all_objects(design)
    expect_true("n_arms" %in% objects$name)
    expect_true(all(c(1L, 2L) %in% objects$step[objects$name == "n_arms"]))
  })
})

test_that("an inquiry's name does not shadow the same name in a later step", {
  local({
    ATE <- 0.5
    design <- declare_model(N = 40, Y = rnorm(N)) +
      declare_inquiry(ATE = ATE) +
      declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                        inquiry = if (ATE > 0) "ATE" else "none")
    objects <- find_all_objects(design)
    expect_true(all(c(2L, 3L) %in% objects$step[objects$name == "ATE"]))
  })
})

test_that("redesigning N in a multilevel model leaves the level sizes alone", {
  # Macartan's case: `N` means the top level's size where the design declares
  # it, and the number of rows the level is building everywhere else. A
  # redesign on the parameter must move the first and not the second.
  local({
    N <- 5
    design <- declare_model(
      villages = declare_level(N = N, v_u = rnorm(N)),
      households = nest_level(N = 3, y = v_u + rnorm(N))
    ) + NULL
    expect_equal(nrow(draw_data(design)), 15L)
    expect_equal(nrow(draw_data(redesign(design, N = 20))), 60L)

    objects <- find_all_objects(design)
    # `N` is the top level's size, reported from the one step that declares it.
    # The second level's `N = 3` is a literal and its `rnorm(N)` is that
    # level's row count, so neither is a parameter of the design.
    expect_equal(unique(objects$step[objects$name == "N"]), 1L)
    expect_equal(unique(objects$value[objects$name == "N"]), "5")
  })
})

test_that("each name is reported with the kind of value it holds", {
  local({
    N <- 40
    probs <- c(0.3, 0.7)
    weights <- matrix(1, 2, 2)
    g <- function(x) mean(x)
    covariates <- data.frame(id = 1:40)
    design <- declare_model(N = N, Y = rnorm(N), W = sample(0:1, N, TRUE,
                                                            prob = probs)) +
      declare_inquiry(mu = g(Y) + sum(weights) + nrow(covariates))
    objects <- unique(find_all_objects(design)[c("name", "kind")])
    kinds <- stats::setNames(objects$kind, objects$name)
    expect_equal(kinds[["N"]], "scalar")
    expect_equal(kinds[["probs"]], "vector")
    expect_equal(kinds[["weights"]], "data")
    expect_equal(kinds[["g"]], "function")
    expect_equal(kinds[["covariates"]], "data")
  })
})

test_that("printing a design lists its parameters", {
  local({
    N <- 30
    design <- declare_model(N = N, Y = rnorm(N)) + NULL
    expect_output(print(design), "Parameters and objects")
    expect_output(print(design), "N")
    # a design with nothing to redesign prints its steps alone
    bare <- declare_model(data = data.frame(x = 1:3), Y = x + 1) + NULL
    out <- paste(utils::capture.output(print(bare)), collapse = "\n")
    expect_false(grepl("Parameters and objects", out))
  })
})

test_that("n() is available in a declaration and is not a parameter", {
  local({
    N <- 20
    design <- declare_model(N = N, Y = rnorm(n()), size = n()) + NULL
    df <- draw_data(design)
    expect_equal(nrow(df), 20L)
    expect_equal(unique(df$size), 20L)
    # `n()` counts the rows in hand, so a redesign moves it without being
    # asked to, and it is not offered as something to redesign.
    expect_equal(unique(draw_data(redesign(design, N = 45))$size), 45L)
    expect_false("n" %in% find_all_objects(design)$name)
  })
})

test_that("the value column is a display snippet, not a serialisation", {
  # Macartan's table showed `prob_each` as fifteen digits of 1/3 three times
  # over, and every function as the word "function". The value itself is
  # reachable through the row's `env`; the column is for reading.
  prob_each <- rep(1/3, 3)
  hdl <- function(data) { data$Y <- rnorm(nrow(data)); data }
  design <- declare_model(N = 30, u = rnorm(N)) +
    declare_assignment(Z = complete_ra(N, prob_each = prob_each)) +
    declare_measurement(handler = hdl)
  params <- design_parameters(design)
  expect_false("value_str" %in% names(params))
  expect_equal(params$value[params$name == "prob_each"], "c(0.333, 0.333, 0.333)")
  fn_row <- params$value[params$name == "hdl"]
  expect_match(fn_row, "^function \\(data\\)")
  expect_lte(nchar(fn_row), 40L)
  expect_match(fn_row, "\\.\\.\\.$")
  expect_equal(DeclareDesign:::describe_value(function(x) x^2), "function (x) x^2")
  expect_equal(DeclareDesign:::describe_value(1:3), "1:3")
  expect_equal(DeclareDesign:::describe_value(c("a", "b")), 'c("a", "b")')
})
