# DeclareDesign

DeclareDesign declares, diagnoses, and redesigns research designs. A design is written as four parts, a model of the world, an inquiry about it, a data strategy, and an answer strategy, joined with `+`; `diagnose_design()` then simulates it and reports bias, power, coverage, and whatever else you ask for, before any data are collected.

```r
library(DeclareDesign)

design <-
  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")

diagnose_design(design)
#> Research design diagnosis
#>   500 simulations
#>   bootstrap standard errors in parentheses (100 replicates)
#>
#>  Design Inquiry Estimator Outcome Term N Sims Mean Estimand Mean Estimate
#>  design     ATE estimator       Y    Z    500          0.20          0.20
#>                                                      (0.00)        (0.01)
#>    Bias SD Estimate   RMSE  Power Coverage
#>    0.00        0.20   0.20   0.17     0.96
#>  (0.01)      (0.01) (0.01) (0.02)   (0.01)
```

`library(DeclareDesign)` also attaches the three companion packages that supply the pieces: [fabricatr](https://declaredesign.org/r/fabricatr/) makes data, [randomizr](https://declaredesign.org/r/randomizr/) randomizes, and [estimatr](https://declaredesign.org/r/estimatr/) estimates. The book *Research Design in the Social Sciences* (Blair, Coppock, and Humphreys) works through all of it at <https://book.declaredesign.org>.

## Installing

Install from CRAN, which brings fabricatr, randomizr, and estimatr 2.0 with it:

```r
install.packages("DeclareDesign")
```

The development version is on [GitHub](https://github.com/DeclareDesign/DeclareDesign):

```r
remotes::install_github("DeclareDesign/DeclareDesign")
```

## What changed in 2.0

2.0 is a ground-up rewrite on tidyverse foundations. The declaration syntax is unchanged, and the book's designs run as written. What changed:

* `redesign()` reaches a name the design reads (a value named above the design, a designer's argument, or `declare_parameters()`), and errors, with the fix, on a number written inside a step.
* `declare_parameters()` and `declare_notes()` name what a design can be set to and what it works out.
* A design is a value: it carries the objects it reads, so it survives `saveRDS()` and travels to parallel workers. A seeded run gives the same numbers sequentially and under `future::plan(multisession)`.
* Step-level `draws` hold a population fixed while the assignment is redrawn, and the diagnosis decomposes variance by step.
* An estimator that fails on one draw is recorded rather than fatal.
* `inquiry =` takes the inquiry's label; `make_groups` is `group_by()`; `print_code()` and `compare_designs()` are gone.

The [changelog](https://declaredesign.org/r/declaredesign/news/) has the full list, and `vignette("declaredesign2.0")` the reasoning, the speed measurements, and a grep table for porting a script. `vignette("getting-started")` is the short introduction.
