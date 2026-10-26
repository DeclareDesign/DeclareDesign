# Attached the way a user attaches them: designs are written with bare
# add_level(), complete_ra(), lm_robust() and if_else() calls, so these have to
# be on the search path for the tests to exercise the path users actually take.
#
# randomizr, fabricatr and estimatr are Depends, so library(DeclareDesign)
# already attached them and a skip_if_not_installed() on any of the three can
# never fire. Seventeen of them were written here anyway, and they read as
# "this test is optional" over tests that are not. The Suggests really are
# optional and are skipped around where they are used: furrr, future,
# progressr, withr, MASS.
library(dplyr)
library(tidyr)
library(purrr)

simple_design <- function(N = 50, ate = 0.3) {
  declare_model(N = N, U = rnorm(N), Y_Z_1 = U + ate, Y_Z_0 = U) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
}
