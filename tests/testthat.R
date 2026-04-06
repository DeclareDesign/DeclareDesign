library(testthat)
library(DeclareDesign)

for (pkg in c("AER", "betareg", "biglm", "coin", "future.apply", "gam", "Matching", "reshape2", "sf")) {
  if (requireNamespace(pkg, quietly = TRUE)) library(pkg, character.only = TRUE)
}

test_check("DeclareDesign")
