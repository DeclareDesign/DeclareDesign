
<!-- README.md is generated from README.Rmd. Please edit that file -->
DeclareDesign: Declare and diagnose research designs to understand and improve them.
====================================================================================

**DeclareDesign** is statistical software that makes it easier for researchers to characterize and learn about the properties of research designs before implementation. Ex ante declaration and diagnosis of designs can help researchers clarify the strengths and limitations of their designs and to improve their properties. It can make it easier for readers to evaluate a research strategy prior to implementation and without access to results. It can also make it easier for designs to be shared, replicated, and critiqued.

The motivation for the software is described in a [working paper](http://declaredesign.org/paper.pdf) by the authors.

**DeclareDesign** consists of a core package, which is documented on this web site, as well as three companion packages that stand on their own but are also called on by the core package. They are:

1.  [randomizr](http://randomizr.declaredesign.org): Easy to use tools for common forms of random assignment and sampling.
2.  [fabricatr](http://fabricatr.declaredesign.org): Imagine your data before you collect it.
3.  [estimatr](http://estimatr.declaredesign.org): Fast estimators for social scientists.

A [library](http://declaredesign.org/articles/design_library.html) of declared designs is under construction. The library includes canonical designs which users can download, modify, and deploy.

**Note:** *This software is in not yet ready for general release. Please contact the authors before using in experiments or published work. Specifications, names, and arguments of functions are subject to change.*

### Installing DeclareDesign

To install the latest development release of all of the packages, please ensure that you are running version 3.3 or later of R and run the following code:

``` r
install.packages("DeclareDesign", dependencies = TRUE, 
                 repos = c("http://R.declaredesign.org", "https://cloud.r-project.org"))
```

This project is generously supported by a grant from the [Laura and John Arnold Foundation](http://www.arnoldfoundation.org) and seed funding from [EGAP](http://egap.org).
