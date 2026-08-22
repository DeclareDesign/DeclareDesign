test_that("a declared parameter drives the design and follows a redesign", {
  local({
    design <- declare_parameters(n_units = 100, effect = 0.5) +
      declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + effect) +
      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
    expect_equal(nrow(draw_data(design)), 100L)
    expect_equal(nrow(draw_data(redesign(design, n_units = 40))), 40L)
    expect_equal(draw_estimands(design)$estimand, 0.5)
    expect_equal(draw_estimands(redesign(design, effect = 2))$estimand, 2)
  })
})

test_that("a parameter declaration is skipped when the design runs", {
  local({
    design <- declare_parameters(k = 3) + declare_model(N = k)
    expect_equal(names(draw_data(design)), "ID")
    expect_equal(nrow(draw_data(design)), 3L)
  })
})

test_that("a later parameter reads the ones declared before it", {
  local({
    design <- declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1],
                                 names = paste0("ate_", ks)) +
      declare_model(N = 10)
    params <- design_parameters(design)
    expect_equal(params$value_str[params$name == "ks"], "2:3")
    expect_equal(params$value_str[params$name == "names"],
                 'c("ate_2", "ate_3")')
    expect_true(all(params$declared[params$name %in% c("m_arms", "ks", "names")]))
  })
})

test_that("a declared parameter beats a binding of the same name in the workspace", {
  local({
    x <- 999
    design <- declare_parameters(x = 2) + declare_model(N = 1, Y = x)
    expect_equal(draw_data(design)$Y, 2)
  })
})

test_that("a redesign changes the parameter and leaves a column of that name alone", {
  local({
    design <- declare_parameters(a = 4) + declare_model(N = a, a = 5)
    expect_equal(nrow(draw_data(design)), 4L)
    expect_equal(unique(draw_data(design)$a), 5)
    changed <- draw_data(redesign(design, a = 3))
    # The parameter moves the row count; the column keeps its own expression.
    expect_equal(nrow(changed), 3L)
    expect_equal(unique(changed$a), 5)
  })
})

test_that("a redesign to the value a parameter already holds changes nothing", {
  local({
    design <- declare_parameters(k = 5, s = 2) +
      declare_model(N = k, Y = rnorm(N, 0, s))
    set.seed(11); before <- draw_data(design)
    set.seed(11); after <- draw_data(redesign(design, k = 5))
    expect_identical(before, after)
    set.seed(11); after_s <- draw_data(redesign(design, s = 2))
    expect_identical(before, after_s)
  })
})

test_that("a parameter reaches a handler that reads it out of its closure", {
  local({
    design <- declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1],
                                 inquiry_names = paste0("ate_", ks),
                                 term_names = paste0("factor(Z)", ks)) +
      declare_model(N = 60, u = rnorm(N)) +
      declare_inquiry(handler = function(data) {
        data.frame(inquiry = inquiry_names, estimand = 0)
      }) +
      declare_assignment(Z = randomizr::complete_ra(N, conditions = seq_len(m_arms))) +
      declare_measurement(Y = u + as.numeric(Z)) +
      declare_estimator(Y ~ factor(Z), .method = estimatr::lm_robust,
                        term = term_names, inquiry = inquiry_names)
    expect_equal(run_design(design)$inquiry, c("ate_2", "ate_3"))
    four <- redesign(design, m_arms = 4)
    out <- run_design(four)
    expect_equal(out$inquiry, c("ate_2", "ate_3", "ate_4"))
    expect_equal(out$term, c("factor(Z)2", "factor(Z)3", "factor(Z)4"))
    expect_false(any(is.na(out$estimate)))
  })
})

test_that("a package function used as a handler is not re-homed", {
  local({
    design <- declare_parameters(k = 2) +
      declare_model(N = 6, pair = rep(1:3, each = 2),
                    role = rep(c("A", "B"), 3), a = 1:6, kk = k) +
      declare_step(id_cols = pair, names_from = role, values_from = c(a),
                   handler = tidyr::pivot_wider)
    expect_equal(names(draw_data(design)), c("pair", "A", "B"))
    expect_equal(names(draw_data(redesign(design, k = 3))), c("pair", "A", "B"))
  })
})

test_that("a second declaration applies only to the steps after it", {
  local({
    design <- declare_parameters(v = 1) +
      declare_model(N = 1, first = v) +
      declare_parameters(v = 9) +
      declare_measurement(second = v)
    df <- draw_data(design)
    expect_equal(df$first, 1)
    expect_equal(df$second, 9)
  })
})

test_that("every parameter must be named exactly once", {
  expect_error(declare_parameters(1), "must be named")
  expect_error(declare_parameters(x = 1, x = 2), "more than once")
})

test_that("a design with no parameter declaration is untouched", {
  local({
    n <- 10
    design <- declare_model(N = n, Y = rnorm(N))
    expect_false(any(design_parameters(design)$declared))
    expect_equal(nrow(draw_data(redesign(design, n = 25))), 25L)
  })
})

test_that("a stochastic declared parameter is fixed for the life of the design", {
  local({
    design <- declare_parameters(u = rnorm(1), v = u * 2) +
      declare_model(N = 2, y = u)
    first <- unique(draw_data(design)$y)
    # `+` rebuilds the design, and so does every redesign. Neither may redraw.
    expect_equal(unique(draw_data(design + NULL)$y), first)
    expect_equal(unique(draw_data(redesign(design, v = 99))$y), first)
    # redesigning to the value it already holds changes nothing at all
    expect_equal(unique(draw_data(redesign(design, u = first))$y), first)
  })
})

test_that("changing a parameter recomputes the ones declared after it and no others", {
  local({
    design <- declare_parameters(a = 2, b = a * 10, c_val = rnorm(1)) +
      declare_model(N = 1, y = b)
    expect_equal(draw_data(design)$y, 20)
    before <- DeclareDesign:::current_param_value(design, "c_val")
    moved <- redesign(design, a = 3)
    expect_equal(draw_data(moved)$y, 30)
    # `c_val` is declared after `a`, so it is invalidated and redrawn
    expect_false(identical(before,
                           DeclareDesign:::current_param_value(moved, "c_val")))
    # but a parameter declared before the change is untouched
    kept <- redesign(design, c_val = 1)
    expect_equal(draw_data(kept)$y, 20)
  })
})
