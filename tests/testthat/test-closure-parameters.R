test_that("a parameter a handler reads out of its closure is found and changed", {
  # Macartan's `b` / `f` / `hdl` case, undeclared. DeclareDesign 1.1.1 reaches
  # it, so this is a regression to close rather than a feature to add.
  local({
    b <- 100
    f <- function(...) fabricatrZero::fabricate(...)
    hdl <- function(...) f(..., extra = rnorm(2, b, 0))
    design <- declare_model(N = 2, U = rnorm(N)) +
      declare_measurement(handler = hdl)
    rm(b, f, hdl)
    expect_setequal(design_parameters(design)$name, c("hdl", "f", "b"))
    expect_equal(draw_data(design)$extra, c(100, 100))
    expect_equal(draw_data(redesign(design, b = 50))$extra, c(50, 50))
    expect_no_warning(redesign(design, b = 50))
  })
})

test_that("a parameter one function deeper than the handler is reached", {
  local({
    k <- 7
    inner <- function(n) rep(k, n)
    outer_h <- function(data) { data$z <- inner(nrow(data)); data }
    design <- declare_model(N = 3) + declare_step(handler = outer_h)
    expect_equal(draw_data(design)$z, rep(7, 3))
    expect_equal(draw_data(redesign(design, k = 1))$z, rep(1, 3))
  })
})

test_that("a .method reads its parameters out of its closure too", {
  skip_if_not_installed("estimatr")
  local({
    shift <- 100
    my_method <- function(formula, data) {
      fit <- estimatr::lm_robust(formula, data = data)
      fit$coefficients <- fit$coefficients + shift
      fit
    }
    design <- declare_model(N = 40, u = rnorm(N), Z = rep(0:1, 20), Y = u + Z) +
      declare_estimator(Y ~ Z, .method = my_method, term = "Z")
    expect_gt(run_design(design)$estimate, 50)
    expect_lt(run_design(redesign(design, shift = 0))$estimate, 50)
  })
})

test_that("a function that reassigns the name upward is left alone", {
  # `<<-` searches from the function's enclosing environment, so re-homing
  # would send the assignment to the copy instead of to the binding the author
  # meant. The one case where re-homing changes what a function means.
  local({
    counter <- 0
    bump <- function(data) { counter <<- counter + 1; data$k <- counter; data }
    design <- declare_model(N = 2) + declare_step(handler = bump)
    expect_s3_class(redesign(design, counter = 99), "design")
    expect_equal(counter, 0)
    invisible(draw_data(design))
    expect_equal(counter, 1)
  })
})

test_that("this package's own closures are not reported as design parameters", {
  # A closure a package *returns* has an ordinary function frame for an
  # environment, and only its ancestors are the namespace. Walking into one
  # reported `term`, `label`, `.method` and `summary_fn` as parameters of 37
  # library designs.
  skip_if_not_installed("estimatr")
  design <- declare_model(N = 20, u = rnorm(N), Z = rep(0:1, 10), Y = u + Z) +
    declare_estimator(Y ~ Z, .method = estimatr::lm_robust, term = "Z")
  found <- design_parameters(design)$name
  expect_false(any(c("term", "label", ".method", "summary_fn", "inquiry_chr")
                   %in% found))
})

test_that("Macartan's twice-reported N and se_type are gone", {
  # His 2026-08-22 example: `design_parameters()` listed `N` at two values and
  # `se_type` at two values, none of which a redesign could coherently change.
  skip_if_not_installed("estimatr")
  skip_if_not_installed("randomizr")
  local({
    N <- 200
    design <-
      declare_model(N = 10, x = 1:N, Y_Z_0 = x^2, Y_Z_1 = x^2 + 10) +
      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
      declare_assignment(Z = randomizr::complete_ra(N)) +
      declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
      declare_estimator(Y ~ Z + x, inquiry = "ATE", label = "1",
                        se_type = "stata") +
      declare_estimator(Y ~ Z + x, inquiry = "ATE", label = "2",
                        se_type = "HC2")
    expect_equal(nrow(design_parameters(design)), 0L)
    expect_equal(nrow(draw_data(design)), 10L)
    # and the refusal is the only thing said, not a "not found" warning first
    expect_error(redesign(design, N = 50), "not a parameter")
    expect_error(suppressWarnings(redesign(design, N = 50)), "not a parameter")
  })
})

test_that("a function passed to .method, .summary or handler is reachable by name", {
  # Six library designs advertised a parameter `redesign()` could not reach,
  # because `.method`, `.summary` and `handler` are formals: the function
  # arrived as a value and its name appeared in no quosure.
  skip_if_not_installed("estimatr")
  local({
    plain <- function(x) tibble::tibble(term = "a", estimate = 1)
    doubled <- function(x) tibble::tibble(term = "a", estimate = 2)
    design <- declare_model(N = 10, u = rnorm(N), Z = rep(0:1, 5), Y = u + Z) +
      declare_estimator(Y ~ Z, .method = estimatr::lm_robust, .summary = plain,
                        term = "a")
    expect_true("plain" %in% design_parameters(design)$name)
    expect_equal(run_design(design)$estimate, 1)
    expect_equal(run_design(redesign(design, plain = doubled))$estimate, 2)
  })
})

test_that("a handler passed by name to declare_step is reachable", {
  local({
    add_one <- function(data) { data$k <- 1; data }
    add_two <- function(data) { data$k <- 2; data }
    design <- declare_model(N = 2) + declare_step(handler = add_one)
    expect_true("add_one" %in% design_parameters(design)$name)
    expect_equal(unique(draw_data(design)$k), 1)
    expect_equal(unique(draw_data(redesign(design, add_one = add_two))$k), 2)
  })
})
