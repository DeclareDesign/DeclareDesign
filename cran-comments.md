## Submission

Patch release fixing CRAN check failures in 1.1.0.

* Replaced the unexported rlang internal `quo_get_env()` with the exported `get_env()` in `R/aaa.R` and `R/declare_potential_outcomes.R`.
* Replaced `enquo()` with `substitute()` in `+.dd` (`R/construct_design.R`) for forward compatibility with the upcoming rlang release, which uses the new R C API to capture expressions. Fix contributed by Lionel Henry.
* Added `CausalQueries`, `rdrobust`, and `rdss` to `Suggests` to resolve CRAN NOTE about undeclared test dependencies.
* Made top-level test setup (`tests/testthat.R`) use soft package loads for suggested packages.
* Improved `print.design()` output: clearer labels, deduplication of parameter names, truncation of long values.
* Added example to `select_diagnosands()` documentation.
* Updated `R (>= 4.1.0)` dependency to reflect use of the native pipe operator.

## Test environments
* local OS X install, R 4.5.2
* win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

No changes to worse.

---
