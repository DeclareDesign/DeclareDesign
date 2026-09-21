# `set_diagnosands()`, `select_diagnosands()`, and the diagnosand library, the
# three things in R/set_diagnosands.R.
#
# `set_diagnosands()` is deprecated in 2.0 and removed in 2.1. It stays
# exported through 2.0 because DesignLibrary 0.1.10 on CRAN carries
# `importFrom(DeclareDesign, set_diagnosands)` and would not load without it,
# so both halves are tested here: that it warns, and that it still works.

test_that("diagnose_design accepts custom diagnosands via set_diagnosands", {
  design <- suppressWarnings(
    simple_design(N = 30) |>
      set_diagnosands(declare_diagnosands(
        mean_estimate = mean(estimate, na.rm = TRUE)
      ))
  )
  d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
  expect_true("mean_estimate" %in% names(get_diagnosands(d)))
  expect_false("bias" %in% names(get_diagnosands(d)))
})

test_that("select_diagnosands subsets diagnosands", {
  diags <- default_diagnosands()
  trimmed <- select_diagnosands(diags, "bias", "rmse")
  expect_equal(names(attr(trimmed, "dots")), c("bias", "rmse"))
})

test_that("select_diagnosands builds a set from the library, as in DeclareDesign", {
  diags <- select_diagnosands("sd_estimate", "mean_se")
  expect_equal(names(attr(diags, "dots")), c("sd_estimate", "mean_se"))
  d <- diagnose_design(simple_design(N = 30), diagnosands = diags, sims = 10,
                       bootstrap_sims = 0)
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("design", "inquiry", "estimator", "outcome", "term",
                         "n_sims")),
               c("sd_estimate", "mean_se"))
})

test_that("select_diagnosands reaches diagnosands outside the default set", {
  extra <- c("type_s_rate", "exaggeration_ratio", "var_estimate",
             "mean_var_hat", "prop_pos_sig", "mean_ci_length", "mean_estimand")
  diags <- rlang::inject(select_diagnosands(!!!extra))
  expect_equal(names(attr(diags, "dots")), extra)
})

test_that("select_diagnosands names an unknown diagnosand rather than ignoring it", {
  expect_error(select_diagnosands("bais"), "Unknown diagnosand")
  expect_error(select_diagnosands(default_diagnosands(), "bais"),
               "not in this set")
})

test_that("select_diagnosands passes alpha through to power", {
  sims <- simulate_design(simple_design(N = 30, ate = 0), sims = 30)
  loose <- diagnose_simulations(sims, bootstrap_sims = 0,
                                diagnosands = select_diagnosands("power", alpha = 1))
  strict <- diagnose_simulations(sims, bootstrap_sims = 0,
                                 diagnosands = select_diagnosands("power", alpha = 0))
  expect_equal(get_diagnosands(loose)$power, 1)
  expect_equal(get_diagnosands(strict)$power, 0)
})

test_that("select_diagnosands refuses library arguments when subsetting a set", {
  diags <- declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
  expect_error(select_diagnosands(diags, "power", alpha = 0.5),
               "cannot be applied to a diagnosands set that already exists")
  expect_error(select_diagnosands(diags, "power", subset = p.value < 1),
               "cannot be applied")
  expect_no_error(select_diagnosands(diags, "power"))
})

test_that("select_diagnosands says so when the step is not a diagnosands set", {
  expect_error(select_diagnosands(declare_model(N = 5, Y = rnorm(N)), "bias"),
               "this is a model step")
})

test_that("a design carrying DeclareDesign diagnosands falls back to the defaults", {
  # Found by running DesignLibrary: two of its designers call
  # DeclareDesign's set_diagnosands(), and the object that leaves on the
  # design has the same class, step_type and causal_type as ours, so only the
  # dots tell them apart. We used to read it and die inside quo_get_expr().
  design <- simple_design(N = 30)
  foreign <- structure(
    function(data) data,
    dots = list(data = quote(data), bias = rlang::quo(mean(estimate - estimand))),
    step_type = "diagnosand", causal_type = "diagnosands",
    class = c("design_step", "dd", "function")
  )
  attr(design, "diagnosands") <- foreign

  expect_warning(d <- diagnose_design(design, sims = 5, bootstrap_sims = 0),
                 "cannot be read here")
  expect_true(all(c("bias", "rmse", "power", "coverage") %in%
                    names(get_diagnosands(d))))
  expect_error(
    diagnose_design(simple_design(N = 30), diagnosands = foreign, sims = 5),
    "not a declare_diagnosands\\(\\) object from this package"
  )
})

test_that("our own set_diagnosands object is still read", {
  design <- suppressWarnings(
    simple_design(N = 30) |>
      set_diagnosands(declare_diagnosands(mean_estimate = mean(estimate)))
  )
  expect_no_warning(d <- diagnose_design(design, sims = 5, bootstrap_sims = 0))
  expect_equal(setdiff(names(get_diagnosands(d)),
                       c("design", "inquiry", "estimator", "outcome", "term",
                         "n_sims")),
               "mean_estimate")
})

test_that("set_diagnosands warns and names the replacement", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  design <- simple_design(N = 30)
  expect_warning(
    set_diagnosands(design, declare_diagnosands(mean_estimate = mean(estimate))),
    "deprecated"
  )
})

test_that("set_diagnosands still attaches the diagnosands it is given", {
  design <- suppressWarnings(
    set_diagnosands(simple_design(N = 30),
                    declare_diagnosands(mean_estimate = mean(estimate)))
  )
  expect_false(is.null(attr(design, "diagnosands")))
})

test_that("redesign warns that it is dropping attached diagnosands", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  design <- suppressWarnings(
    set_diagnosands(simple_design(N = 30),
                    declare_diagnosands(mean_estimate = mean(estimate)))
  )
  expect_warning(redesign(design, N = 50), "drops the diagnosands")
})

test_that("a redesigned design is diagnosed with the defaults, not the attached set", {
  # The reason the warning above has to exist. When the estimator's output
  # cannot support the defaults the diagnosis is a table of NAs, not an error:
  # DesignLibrary's process_tracing_designer emits `posterior_H` and no
  # `estimate`, so every default diagnosand is NA after one redesign().
  design <- suppressWarnings(
    set_diagnosands(simple_design(N = 30),
                    declare_diagnosands(mean_estimate = mean(estimate)))
  )
  redesigned <- suppressWarnings(redesign(design, N = 50))
  expect_null(attr(redesigned, "diagnosands"))
  diagnosands <- names(get_diagnosands(
    diagnose_design(redesigned, sims = 3, bootstrap_sims = 0)
  ))
  # The attached set was one diagnosand; the full default seven come back.
  expect_true(all(names(attr(default_diagnosands(), "dots")) %in% diagnosands))
})

test_that("select_diagnosands() needs at least one name", {
  expect_error(select_diagnosands(), "Name at least one diagnosand to keep")
  expect_error(select_diagnosands(default_diagnosands()),
               "Name at least one diagnosand to keep")
})

test_that("a design carries the citation it was given", {
  design <- declare_model(N = 10, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
  expect_null(cite_design(design))
  cited <- set_citation(design, title = "Example", author = "Coppock", year = 2026)
  expect_equal(cite_design(cited),
               list(title = "Example", author = "Coppock", year = 2026))
  expect_s3_class(cited, "design")
  expect_equal(names(cited), names(design))
})
