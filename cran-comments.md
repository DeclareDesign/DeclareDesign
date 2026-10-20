> **HOLD. Do not submit yet.** fabricatr 2.0.0 must be on CRAN first, the
> `Remotes:` field must come out of DESCRIPTION, and win-builder has not been
> run. Delete this banner once all three are done.

## Submission

DeclareDesign 2.0.0 is a reimplementation of the package on tidyverse primitives. The declaration syntax is unchanged; the changes are at the edges, and each is listed in NEWS.md with the reasoning in `vignette("declaredesign2.0")`. Retired arguments and functions stop with an error naming the replacement rather than failing somewhere downstream.

It requires fabricatr (>= 2.0.0), estimatr (>= 2.0.0), and randomizr (>= 1.0.1). estimatr 2.0.1 and randomizr 2.0.1 are on CRAN, and fabricatr 2.0.0 was accepted on <DATE>. The fabricatr submission told CRAN this release would follow it.

This version was written by the maintainers working with AI assistance (Claude, from Anthropic), and `vignette("declaredesign2.0")` says so. The evidence for the release: a suite of 1106 assertions, and an acceptance suite (`tests/acceptance/`, run outside `R CMD check`) that declares and executes 84 of the 90 design declarations in Blair, Coppock, and Humphreys, *Research Design in the Social Sciences* (Princeton University Press, 2023), 81 of them verbatim, and fails any whose estimator returns only `NA`.

## Test environments

* local macOS 26.6 (aarch64, Apple M4), R 4.6.0
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release). <PENDING: first run with the devel job; the four-job run 39166729091 was green at `FAIL 0 | WARN 0 | SKIP 0 | PASS 1106`.>
* win-builder: not yet run

## R CMD check results

<PENDING: re-run once fabricatr 2.0.0 is on CRAN and Remotes is removed. The local check of 2026-10-19 gave 0 errors | 1 warning | 0 notes, the warning being this machine's randomizr binary built under R 4.6.1.>

## Reverse dependencies

`revdepcheck` was run against the submitted code on 2026-10-19, with fabricatr 2.0.0 installed for the new version: 7 checked, 5 with no new problems, 2 newly broken, none failed to check. `CausalQueries`, `estimatr`, `marginaleffects`, `ordbetareg`, and `randomizr` are unaffected. Both packages that break are maintained by the DeclareDesign authors, and both breaks are documented 2.0 changes.

`rdss` 1.0.14 fails eight test assertions, all of them one argument: each test calls `simulate_design(..., future.seed = FALSE)`. 2.0 drops `future.seed`, because every draw now runs on its own L'Ecuyer-CMRG stream under any `future::plan()`, and passing it stops with an error saying so. <PENDING: rdss plan>

`DesignLibrary` 0.1.10 already fails its tests on CRAN. Under 2.0 its designers also fail to build, because they declare potential outcomes with `declare_potential_outcomes()`, which 2.0 makes defunct with an error showing the `declare_model()` form to write instead. We will release a fixed `DesignLibrary` once fabricatr 2.0.0 and DeclareDesign 2.0.0 are both on CRAN, since the fix is written against them.
