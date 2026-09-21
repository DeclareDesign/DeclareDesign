# The acceptance corpus: every design declaration in the DeclareDesign book,
# run end to end, plus the book's worked examples.
#
# It is a second test target rather than more files in tests/testthat/ because
# it answers a different question and is measured differently. tests/testthat/
# asks whether each piece of the package does what it says; this asks whether
# designs people actually wrote still run and still produce estimates. It is
# 84 declarations needing a dozen modelling packages, it skips entirely on
# CRAN, and on 2026-09-21 it was measured at 0.36 percentage points of line
# coverage over what tests/testthat/ already reaches. Mixing the two made "the
# suite" ambiguous in this package: 901 assertions with NOT_CRAN set against
# 711 without, from one directory, with no way to say which half moved.
library(testthat)
library(DeclareDesign)

test_dir("acceptance", package = "DeclareDesign", load_package = "installed")
