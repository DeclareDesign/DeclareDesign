library(testthat)
library(DeclareDesign)

pacman::p_load(AER, betareg, biglm, coin, future.apply, gam, Matching, reshape2, sf, tidyr, purrr, glue)
test_check("DeclareDesign")
