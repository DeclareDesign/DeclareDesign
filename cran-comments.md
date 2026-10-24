## Submission

DeclareDesign 2.0.0 is a reimplementation of the package on tidyverse primitives. The declaration syntax is unchanged; the changes are at the edges, and each is listed in NEWS.md with the reasoning in `vignette("declaredesign2.0")`. Retired arguments and functions stop with an error naming the replacement rather than failing somewhere downstream.

It requires fabricatr (>= 2.0.0), estimatr (>= 2.0.0), and randomizr (>= 1.0.1). estimatr 2.0.1 and randomizr 2.0.1 are on CRAN, and fabricatr 2.0.0 was published on 2026-10-21. The fabricatr submission told CRAN this release would follow it.

This version was written by the maintainers working with AI assistance (Claude, from Anthropic), and `vignette("declaredesign2.0")` says so. The evidence for the release: a suite of 1268 assertions under `R CMD check` (1282 with `NOT_CRAN` set, as on GitHub Actions), which declares and executes 72 of the 90 design declarations in Blair, Coppock, and Humphreys, *Research Design in the Social Sciences* (Princeton University Press, 2023) and fails any whose estimator returns only `NA`. A separate acceptance suite (`tests/acceptance/`, skipped on CRAN) does the same for 12 more that need modelling packages outside this package's dependencies.

## Test environments

* local macOS 26.6 (aarch64, Apple M4), R 4.6.1: 0 errors | 0 warnings | 0 notes
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release). All five green at `FAIL 0 | WARN 0 | SKIP 0 | PASS 1282` (run 40108321491, with every dependency from CRAN).
* win-builder: R-devel (2026-10-23 r90760 ucrt), 1 NOTE

## R CMD check results

0 errors | 0 warnings | 1 note

* "Possibly misspelled words in DESCRIPTION: Tidyverse, tidyverse." Both are correct: tidyverse is the name of the collection of R packages the rewrite is built on.

## Reverse dependencies

`revdepcheck` was run against the submitted code on 2026-10-19, with fabricatr 2.0.0 installed for the new version: 7 checked, 5 with no new problems, 2 newly broken, none failed to check. `CausalQueries`, `estimatr`, `marginaleffects`, `ordbetareg`, and `randomizr` are unaffected. Both packages that break are maintained by the DeclareDesign authors, and both breaks are documented 2.0 changes.

`rdss` 1.0.14 fails eight test assertions, all of them one argument: each test calls `simulate_design(..., future.seed = FALSE)`. 2.0 drops `future.seed`, because every draw now runs on its own L'Ecuyer-CMRG stream under any `future::plan()`, and passing it stops with an error saying so. `rdss` 1.0.16, on GitHub, calls `simulate_design()` with `future.seed` only when the installed DeclareDesign accepts it, and passes its tests under 2.0.0. We will submit it once this release is accepted.

`DesignLibrary` 0.1.10 already fails its tests on CRAN. Under 2.0 its designers also fail to build, because they declare potential outcomes with `declare_potential_outcomes()`, which 2.0 makes defunct with an error showing the `declare_model()` form to write instead. We will release a fixed `DesignLibrary` once fabricatr 2.0.0 and DeclareDesign 2.0.0 are both on CRAN, since the fix is written against them.
