# R/step_packages.R: a step records the names it reads from attached packages,
# and the entry points warn when the session running the design cannot find
# them (issue #299).
#
# Declarations are evaluated in globalenv() for the reason test-capture.R gives:
# inside test_that() they would resolve through the package namespace, which is
# exactly the case that records nothing.

attach_fake_package <- function(env = parent.frame()) {
  attach(list(fake_upper = function(x) toupper(x),
              fake_scale = function(x) x * 10),
         name = "package:ddfakepkg", warn.conflicts = FALSE)
  withr::defer(
    if ("package:ddfakepkg" %in% search()) detach("package:ddfakepkg"),
    envir = env
  )
}

declare_globally <- function(expr) eval(substitute(expr), envir = globalenv())

warnings_of_class <- function(code, class) {
  caught <- list()
  withCallingHandlers(
    code,
    warning = function(w) {
      if (inherits(w, class)) {
        caught[[length(caught) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    }
  )
  caught
}

test_that("a step records a name it reads from an attached package", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = fake_upper(letters[1:N]))
  )
  expect_equal(attr(design, "packages")[["fake_upper"]], "ddfakepkg")
})

test_that("a name written with `::` is not recorded", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = toupper(letters[1:N]), U = stats::rnorm(N))
  )
  expect_false("rnorm" %in% names(attr(design, "packages")))
  expect_false("toupper" %in% names(attr(design, "packages")))
})

test_that("a package call inside a user helper is recorded", {
  attach_fake_package()
  assign("helper_299", function(x) fake_scale(x) + 1, envir = globalenv())
  withr::defer(rm("helper_299", envir = globalenv()))
  design <- declare_globally(declare_model(N = 5, Y = helper_299(1:N)))
  expect_equal(attr(design, "packages")[["fake_scale"]], "ddfakepkg")
})

test_that("a design read back without the package warns before it fails", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = fake_upper(letters[1:N]))
  )
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(design, f)
  detach("package:ddfakepkg")
  loaded <- readRDS(f)
  expect_warning(
    expect_error(draw_data(loaded), "fake_upper"),
    class = "dd_unattached_packages"
  )
  expect_warning(try(draw_data(loaded), silent = TRUE), "library\\(ddfakepkg\\)")
  err <- suppressWarnings(tryCatch(draw_data(loaded), error = identity))
  expect_match(conditionMessage(err), "`fake_upper` came from ddfakepkg")
  expect_match(conditionMessage(err), "library\\(ddfakepkg\\)")

  attach_fake_package()
  expect_no_warning(draw_data(loaded))
})

attach_other_package <- function(env = parent.frame()) {
  attach(list(fake_upper = function(x) tolower(x)),
         name = "package:ddotherpkg", warn.conflicts = FALSE)
  withr::defer(
    if ("package:ddotherpkg" %in% search()) detach("package:ddotherpkg"),
    envir = env
  )
}

test_that("a name that now resolves to another package warns", {
  # The dplyr-over-stats case: detach the package that masked, and the name
  # still resolves, to the wrong function.
  attach_other_package()
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = fake_upper(letters[1:N]))
  )
  detach("package:ddfakepkg")
  expect_warning(draw_data(design), "now resolves to ddotherpkg")
  expect_warning(draw_data(design), "library\\(ddfakepkg\\)")
})

test_that("a name masked by a later attach warns with the qualified name", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = fake_upper(letters[1:N]))
  )
  attach_other_package()
  expect_warning(draw_data(design), "ddfakepkg::fake_upper")
})

test_that("draw_data() says nothing about a package only the inquiry needs", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 20, Y = rnorm(N)) +
      declare_inquiry(mu = fake_scale(mean(Y)))
  )
  detach("package:ddfakepkg")
  expect_no_warning(draw_data(design))
  expect_warning(try(run_design(design), silent = TRUE),
                 class = "dd_unattached_packages")
})

test_that("simulate_design() warns once, not once per design or simulation", {
  attach_fake_package()
  assign("n_299", 10, envir = globalenv())
  withr::defer(rm("n_299", envir = globalenv()))
  design <- declare_globally(
    declare_model(N = n_299, Y = fake_scale(rnorm(N))) +
      declare_inquiry(mu = mean(Y))
  )
  designs <- redesign(design, n_299 = c(10, 20))
  detach("package:ddfakepkg")
  caught <- warnings_of_class(
    try(simulate_design(designs, sims = 3), silent = TRUE),
    "dd_unattached_packages"
  )
  expect_length(caught, 1L)
})

test_that("a design saved before steps recorded packages runs silently", {
  attach_fake_package()
  design <- declare_globally(
    declare_model(N = 5, lab = fake_upper(letters[1:N]))
  )
  attr(design, "packages") <- NULL
  detach("package:ddfakepkg")
  expect_no_warning(try(draw_data(design), silent = TRUE))
})
