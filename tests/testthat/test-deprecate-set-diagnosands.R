# `set_diagnosands()` is deprecated in 2.0 and removed in 2.1. It stays
# exported through 2.0 because DesignLibrary 0.1.10 on CRAN carries
# `importFrom(DeclareDesign, set_diagnosands)` and would not load without it.

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
