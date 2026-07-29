# Attached the way a user attaches them: designs are written with bare
# add_level(), complete_ra(), lm_robust() and if_else() calls, so these have to
# be on the search path for the tests to exercise the path users actually take.
# The Suggests are attached only if present; tests that need one say so with
# skip_if_not_installed().
library(fabricatrZero)
library(dplyr)
library(tidyr)
library(purrr)
# randomizr and estimatr are Suggests and carry the ordinary tests. The rest
# are needed only by test-book-designs.R, which is skipped on CRAN, so they are
# attached when present and never declared as dependencies.
for (pkg in c("randomizr", "estimatr", "rdss", "stringr", "margins", "bbmle",
              "MatchIt", "broom.mixed", "grf", "spdep", "DIDmultiplegt",
              "CausalQueries", "rstanarm", "cjoint", "lme4", "rdrobust",
              "sf", "marginaleffects", "metafor", "MASS")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressMessages(library(pkg, character.only = TRUE))
  }
}

simple_design <- function(N = 50, ate = 0.3) {
  declare_model(N = N, U = rnorm(N), Y_Z_1 = U + ate, Y_Z_0 = U) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
}
