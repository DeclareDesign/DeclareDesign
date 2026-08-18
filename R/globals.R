utils::globalVariables(c(
  ".data", ".env",
  "estimate", "estimand", "p.value", "conf.low", "conf.high",
  "term", "sd", "estimator", "inquiry", "design", "outcome",
  "std.error", "alpha", "na.rm",
  "bootstrap_id", "sim_ID",
  "var_total", ".y", ".level_mean", ".within_var"
))

#' @importFrom estimatrZero lm_robust
#' @importFrom fabricatrZero fabricate potential_outcomes reveal_outcomes
#' @importFrom randomizr complete_ra
#' @importFrom rlang .data
#' @importFrom stats coef quantile setNames
#' @importFrom rlang :=
NULL
