## Submission

Patch release fixing CRAN check failures in 1.1.0. Replaced the unexported rlang internal function `quo_get_env()` with the exported equivalent `get_env()` in two files (`R/aaa.R` and `R/declare_potential_outcomes.R`). This caused 9 test failures and runtime errors on CRAN when rlang updated its export list.

## Test environments
* local OS X install, R 4.5.2
* win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

No changes to worse.

---
