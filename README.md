# DeclareDesign, rewritten

**This branch is not the CRAN package and it is not `main`.** It holds a ground-up rewrite of DeclareDesign on tidyverse foundations. The released DeclareDesign is unaffected by anything here.

The package on this branch is still named `DeclareDesignZero`, so installing it leaves your CRAN DeclareDesign in place and both can be loaded in the same session. It imports `fabricatrZero`, which comes from the sibling branch and installs alongside your CRAN fabricatr the same way.

```r
remotes::install_github("DeclareDesign/fabricatr@rewrite")
remotes::install_github("DeclareDesign/DeclareDesign@rewrite", build_vignettes = TRUE)
vignette("declaredesign2.0")
```

The vignette is the document to read first. It covers what does not change, what changes and why, how to port an existing declaration, and the one open API question.

## What it is

3,982 lines of R against DeclareDesign's 7,036, using purrr for simulation and dplyr for diagnosis. The exported API is DeclareDesign's, minus the three aliases `declare_estimators`, `declare_potential_outcomes` and `declare_reveal`, plus `default_diagnosands`, `diagnose_simulations`, `merge_estimates_inquiries` and `tidy.diagnosis`.

**The compatibility claim is an artifact rather than a description.** All 28 chapters of book.declaredesign.org were scraped down to 90 design declarations and checked in as `tests/testthat/test-book-designs.R`. Of the 84 that are testable and working under DeclareDesign 1.1.1, **81 work here and 78 work with their text unchanged**. Separately, 18 of DesignLibrary's 19 designers run under all three verbs, and Macartan's 275-expression crash course runs end to end with zero errors, unmodified.

## Status

As of 2026-07-30: 538 tests passing, `R CMD check` 0 errors / 0 warnings / 0 notes.

Two API decisions are open and both change what a 2.0.0 would mean:

- **How estimator arguments reach `.method`** (DeclareDesign issue #463, open since 2021). DeclareDesign passes expressions, which breaks `metafor::rma.uni` and works for the shim written around that breakage; this branch passes values, which does the reverse. Neither convention serves both tidyselect handlers and ordinary R functions. The recommendation in the vignette is an explicit argument naming the convention, defaulting to values.
- **Whether a diagnosands set should stop being a `design_step`.** Implemented on the `rewrite-diagnosands-proposal` branch, deliberately unmerged.

The intent is that this becomes DeclareDesign 2.0.0 after fabricatr 2.0.0 reaches CRAN, since the dependency forces that order. Nothing on this branch asserts it: the DESCRIPTION still reads `DeclareDesignZero 0.0.1`.

Sibling branches: `DeclareDesign/fabricatr@rewrite` and `DeclareDesign/estimatr@rewrite`.
