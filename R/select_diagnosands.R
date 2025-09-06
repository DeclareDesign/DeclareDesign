#' Select diagnosands
#'
#' Helper to declare a set of named diagnosands
#' @param ... Requested diagnosands from the extended diagnosand list
#' @param alpha Significance level, passed to declare_diagnosands
#' @param subset Subset indicator, passed to declare_diagnosands
#' @param na.rm Logical for handling NAs when calculating diagnosands
#' 
#' @details
#' The diagnosands available from the extended diagnosand list are:
#' \preformatted{
#' 
#' | Diagnosand        | Meaning                                          | Formula                                                     |
#' |-------------------|--------------------------------------------------|-------------------------------------------------------------|
#' | mean_estimand     | Expected value of the estimand                   | `mean(estimand)`                                            |
#' | mean_estimate     | Expected value of the estimate                   | `mean(estimate)`                                            |
#' | bias              | (Expected) bias                                  | `mean(estimate - estimand)`                                 |
#' | sd_estimate       | The s.d. of the estimate ('true' standard error) | `sd(estimate)`                                              |
#' | rmse              | The root mean squared error                      | `sqrt(mean((estimate - estimand) ^ 2))`                     |
#' | power             | Power: probability of rejecting a null of zero   | `mean(p.value <= alpha)`                                    |
#' | coverage          | Coverage: prob the CI will cover the truth       | `mean(estimand <= conf.high & estimand >= conf.low)`        |
#' | mean_se           | The expected standard error                      | `mean(std.error)`                                           |
#' | type_s_rate       | Prob a significant estimate has wrong sign       | `mean((sign(estimate) != sign(estimand))[p.value <= alpha])`|
#' | exaggeration_ratio| The expected exaggeration (given significance)   | `mean((estimate/estimand)[p.value <= alpha])`               |
#' | var_estimate      | The variance of the estimate                     | `pop.var(estimate)`                                         |
#' | mean_var_hat      | Expected estimate of the 'true' var of estimate  | `mean(std.error^2)`                                         |
#' | prop_pos_sig      | Probability of a positive estimate given sig.    | `mean(estimate > 0 & p.value <= alpha)`                     |
#' | mean_ci_length    | The expected length of the confidence interval   | `mean(conf.high - conf.low)`                                |
#' } 
#' 
#' @importFrom rlang inject
#' @export
#' @examples
#' select_diagnosands("bias")
#' select_diagnosands("prop_pos_sig", "mean_se", alpha = .001)
#' select_diagnosands("bias")(data.frame(estimate = 2, estimand = 3))
#' select_diagnosands("bias")(data.frame(estimate = 1:2, estimand = c(NA, 2)))
#' select_diagnosands("bias", na.rm = TRUE)(data.frame(estimate = 1:2, estimand = c(NA, 2)))


select_diagnosands <- function(..., alpha = 0.05, subset = NULL, na.rm = FALSE) {
  
  x <- list(...) |> unlist()
  
  all_diagnosands <- list(
    mean_estimand = expr(mean(estimand, na.rm = na.rm)),
    mean_estimate = expr(mean(estimate, na.rm = na.rm)),
    bias = expr(mean(estimate - estimand, na.rm = na.rm)),
    sd_estimate = expr(sd(estimate, na.rm = na.rm)),
    rmse = expr(sqrt(mean((estimate - estimand) ^ 2, na.rm = na.rm))),
    power = expr(mean(p.value <= alpha)),
    coverage = expr(mean(estimand <= conf.high & estimand >= conf.low)),
    mean_se = expr(mean(std.error, na.rm = na.rm)),
    type_s_rate = expr(mean((sign(estimate) != sign(estimand))[p.value <= alpha], na.rm = na.rm)),
    exaggeration_ratio = expr(mean((estimate/estimand)[p.value <= alpha], na.rm = na.rm)),
    var_estimate = expr(pop.var(estimate, na.rm = na.rm)),
    mean_var_hat = expr(mean(std.error^2, na.rm = na.rm)),
    prop_pos_sig = expr(mean(estimate > 0 & p.value <= alpha, na.rm = na.rm)),
    mean_ci_length = expr(mean(conf.high - conf.low, na.rm = na.rm))
  )
  
  
  if(!all(x %in% names(all_diagnosands))){
    message(paste0("Please only request names in set : ", 
                   paste(names(all_diagnosands), collapse = ", "),
                   ". See ?select_diagnosands for extended diagnosand list.\n",
                   "Default diagnosands calculated"))
    
    return(NULL)
  } 
  
  selected_diagnosands <- all_diagnosands[x]
  
  # pass arguments
  inject(declare_diagnosands(!!!selected_diagnosands, alpha = alpha, subset = subset))
  
}
