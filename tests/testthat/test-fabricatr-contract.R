# What this package needs fabricatr::fabricate_with_dots() to do.
#
# Every declaration verb that builds or augments data goes through
# `make_fabricate_step()` in `R/declare_model.R`, and both of its call sites
# call `fabricatr::fabricate_with_dots()` by name with named arguments. The
# function is called by nothing inside fabricatr, so the only thing standing
# between a change there and a run-time failure in every design here is a
# test. fabricatr's own `test-declaredesign-api.R` asserts the function
# behaves; nothing in this package asserted that we can still call it, so a
# rename stayed green on the fabricatr side and would have failed at run time
# in every design here.
#
# It was reached with `:::` until 2026-09-21, and `R CMD check` was silent
# about that only because both packages named the same maintainer, which is a
# fact about authorship rather than about the boundary.
#
# The tests come in two layers, and both are needed. The first layer calls
# fabricatr directly, the way `make_fabricate_step()` does, so a breakage
# reports as the contract clause it broke. The second layer asserts the
# design-level behaviour that rests on each clause, so a change that keeps the
# clause and moves the result is caught too.

# The boundary itself ----

test_that("fabricate_with_dots() is exported and callable with `::`", {
  expect_true("fabricate_with_dots" %in% getNamespaceExports("fabricatr"))
  expect_true(is.function(fabricatr::fabricate_with_dots))
})

test_that("the argument names are the ones both call sites use", {
  # `R/declare_model.R` calls with data =, dots = and ID_label = at both
  # sites, so a rename or reordering of any of the three breaks them.
  expect_equal(names(formals(fabricatr::fabricate_with_dots)),
               c("data", "dots", "ID_label"))
  expect_equal(formals(fabricatr::fabricate_with_dots)$ID_label, "ID")
})

# The clauses make_fabricate_step() relies on ----

test_that("a captured dots list is used as captured, not re-captured", {
  # This is the whole reason the entry point exists. The declaration verbs
  # capture their own arguments so that a design carries its expressions as a
  # value; splicing those captures back into fabricate() with `!!!` renders
  # them as formulas and fabricate()'s enquos() captures the formulas, one
  # level too deep.
  make_dots <- function() {
    k <- 7
    rlang::quos(N = 2, Y = k)
  }
  out <- fabricatr::fabricate_with_dots(dots = make_dots())
  expect_equal(out$Y, c(7, 7))
})

test_that("a quosure named N sets the row count and is not a column", {
  out <- fabricatr::fabricate_with_dots(dots = rlang::quos(N = 3, Y = 1:3))
  expect_equal(nrow(out), 3L)
  expect_false("N" %in% names(out))
})

test_that("ID_label = NA suppresses the id column and keeps the rows", {
  # `make_fabricate_step(id_label_na = TRUE)` and `make_sampling_step()` both
  # pass NA here. fabricate(N = 3, ID_label = NA) returned a 0 x 0 tibble
  # until 2026-09-20, so the row count is as much of the clause as the column
  # names are.
  out <- fabricatr::fabricate_with_dots(dots = rlang::quos(N = 3, Y = 1:3),
                                        ID_label = NA)
  expect_equal(nrow(out), 3L)
  expect_equal(names(out), "Y")
})

test_that("ID_label = \"ID\" appends the id column", {
  out <- fabricatr::fabricate_with_dots(dots = rlang::quos(N = 3, Y = 1:3),
                                        ID_label = "ID")
  expect_equal(names(out), c("ID", "Y"))
})

test_that("data = augments the frame it is handed", {
  base <- fabricatr::fabricate_with_dots(dots = rlang::quos(N = 3, Y = 1:3))
  out <- fabricatr::fabricate_with_dots(data = base,
                                        dots = rlang::quos(Y2 = Y * 2),
                                        ID_label = NA)
  expect_equal(nrow(out), 3L)
  expect_equal(names(out), c("ID", "Y", "Y2"))
  expect_equal(out$Y2, c(2, 4, 6))
})

# What the clauses buy at the level of a design ----

test_that("a model step names its rows", {
  dat <- draw_data(declare_model(N = 3, Y = rnorm(N)))
  expect_equal(names(dat), c("ID", "Y"))
  expect_equal(nrow(dat), 3L)
})

test_that("a step that supplies its own rows does not append a second id", {
  # 1.x wrote ID_label = NA at each of the four call sites in
  # declare_assignment.R, declare_measurement.R, declare_potential_outcomes.R
  # and declare_sampling.R. Here it is the id_label_na flag, which was forced
  # and never passed on until 2026-09-20, so these steps got the right answer
  # only because they normally run with data already in hand.
  dat <- draw_data(declare_measurement(N = 4, Y = rnorm(N)))
  expect_equal(names(dat), "Y")
  expect_equal(nrow(dat), 4L)
})

test_that("measurement, assignment and sampling add columns without an id", {
  model <- declare_model(N = 6, Y = rnorm(N))
  expect_equal(names(draw_data(model + declare_measurement(Y2 = Y * 2))),
               c("ID", "Y", "Y2"))
  expect_equal(names(draw_data(model + declare_assignment(Z = complete_ra(N)))),
               c("ID", "Y", "Z"))
  sampled <- draw_data(model + declare_sampling(S = complete_rs(N, n = 3)))
  expect_equal(names(sampled), c("ID", "Y", "S"))
  expect_equal(nrow(sampled), 3L)
})

test_that("a design's expressions read the environment they were written in", {
  # The same no-double-quoting clause as above, reached through the verb. A
  # design is a value and carries the objects its expressions read, so a
  # local that has gone out of scope still resolves.
  build <- function() {
    ate <- 0.25
    declare_model(N = 4, Y_Z_0 = 0, Y_Z_1 = ate)
  }
  dat <- draw_data(build())
  expect_equal(unique(dat$Y_Z_1), 0.25)
})
