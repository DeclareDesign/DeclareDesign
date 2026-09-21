# The book's designs are written the way a reader writes them, with bare
# mutate(), add_level() and lm_robust() calls, so the packages that supply
# those verbs have to be on the search path. randomizr, fabricatr and estimatr
# are DeclareDesign's own Depends and library(DeclareDesign) attached them
# already; dplyr, tidyr and purrr are Imports, so they are installed but not
# attached, and a design that calls mutate() needs them here.
library(dplyr)
library(tidyr)
library(purrr)

# The packages the book's designs are written against. randomizr, fabricatr
# and estimatr are DeclareDesign's own Depends and are already attached; the
# rest are modelling packages that have no place in this package's
# dependencies, so they are attached when present and skipped around when not.
for (pkg in c("rdss", "stringr", "margins", "bbmle", "MatchIt", "broom.mixed",
              "grf", "spdep", "DIDmultiplegt", "CausalQueries", "rstanarm",
              "cjoint", "lme4", "rdrobust", "sf", "marginaleffects",
              "metafor", "MASS")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressMessages(library(pkg, character.only = TRUE))
  }
}
