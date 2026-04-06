## Submission

Patch release fixing CRAN check failures in 1.1.0.

* Replaced the unexported rlang internal `quo_get_env()` with the exported `get_env()` in `R/aaa.R` and `R/declare_potential_outcomes.R`.
* Replaced `enquo()` with `substitute()` in `+.dd` (`R/construct_design.R`) for forward compatibility with the upcoming rlang release, which uses the new R C API to capture expressions. Fix contributed by Lionel Henry.
* Added `CausalQueries`, `rdrobust`, and `rdss` to `Suggests` to resolve CRAN NOTE about undeclared test dependencies.
* Made top-level test setup (`tests/testthat.R`) use soft package loads for suggested packages.

## Test environments
* local OS X install, R 4.5.2
* win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 2 warnings | 0 notes

The 2 warnings are pre-existing and unrelated to this patch:
* Undocumented `select_diagnosands` (internal function exported without docs).
* Rd/code mismatches for functions documented but no longer present in code.

## Reverse dependencies

No changes to worse.

---
