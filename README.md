# DeclareDesign

DeclareDesign declares, diagnoses and redesigns research designs. A design is written as four parts, a model of the world, an inquiry about it, a data strategy and an answer strategy, joined with `+`; `diagnose_design()` then simulates it and reports bias, power, coverage and whatever else you ask for, before any data are collected.

```r
library(DeclareDesign)

design <-
  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")

diagnose_design(design)
```

The companion packages supply the pieces: [fabricatr](https://github.com/DeclareDesign/fabricatr) makes data, [randomizr](https://github.com/DeclareDesign/randomizr) randomizes, and [estimatr](https://github.com/DeclareDesign/estimatr) estimates. The book *Research Design in the Social Sciences* (Blair, Coppock and Humphreys) works through all of it at <https://book.declaredesign.org>.

## Installing

Version 2.0 is on this repository's `rewrite` branch and not yet on CRAN. It installs under the released name and replaces the CRAN DeclareDesign in your library, as do fabricatr 2.0 and estimatr 2.0, which it needs:

```r
remotes::install_github("DeclareDesign/fabricatr@rewrite")
remotes::install_github("DeclareDesign/estimatr@rewrite")
remotes::install_github("DeclareDesign/DeclareDesign@rewrite", build_vignettes = TRUE)
```

To keep the CRAN versions for comparison, install them into a separate library and pass `lib.loc`.

## What changed in 2.0

2.0 is a ground-up rewrite on tidyverse foundations. The declaration syntax is unchanged, and the book's designs run as written. What changed:

* `redesign()` reaches a name the design reads (a value named above the design, a designer's argument, or `declare_parameters()`), and errors, with the fix, on a number written inside a step.
* `declare_parameters()` and `declare_notes()` name what a design can be set to and what it works out.
* A design is a value: it carries the objects it reads, so it survives `saveRDS()` and travels to parallel workers. A seeded run gives the same numbers sequentially and under `future::plan(multisession)`.
* Step-level `draws` hold a population fixed while the assignment is redrawn, and the diagnosis decomposes variance by step.
* An estimator that fails on one draw is recorded rather than fatal.
* `inquiry =` takes the inquiry's label; `make_groups` is `group_by()`; `print_code()` and `compare_designs()` are gone.

`NEWS.md` has the full list, and `vignette("declaredesign2.0")` the reasoning, the speed measurements and a grep table for porting a script. `vignette("getting-started")` is the short introduction.
