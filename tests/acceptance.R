# The acceptance corpus: the design declarations in the DeclareDesign book
# that need modelling packages from outside DeclareDesign's dependencies.
#
# The book's other 72 declarations need nothing beyond those dependencies and
# run in tests/testthat/test-book-designs.R, unskipped, so CRAN runs them.
# These 12 stay a second test target because they skip entirely on CRAN, and
# mixing skipped tests into tests/testthat/ made "the suite" ambiguous in this
# package: 901 assertions with NOT_CRAN set against 711 without, from one
# directory, with no way to say which half moved.
library(testthat)
library(DeclareDesign)

test_dir("acceptance", package = "DeclareDesign", load_package = "installed")
