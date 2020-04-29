#' Declare test
#'
#' @description Declares an test which generates a test statistic and associated inferential statistics. 
#' 
#' Use of \code{declare_test} is identical to use of \code{\link{declare_estimator}}. Use \code{declare_test} for hypothesis testing with no specific estimand in mind; use \code{declare_estimator} for hypothesis testing when you can link each estimate to an estimand. For example, \code{declare_test} could be used for a K-S test of distributional equality and \code{declare_estimator} for a difference-in-means estimate of an average treatment effect.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
#'
#' @return A function that accepts a data.frame as an argument and returns a data.frame containing the value of the test statistic and other inferential statistics.
#'
#' @examples
#' 
#' # Balance test F test
#' 
#' balance_test_design <-
#'   declare_population(N = 100, cov1 = rnorm(N), cov2 = rnorm(N), cov3 = rnorm(N)) +
#'   declare_assignment(prob = 0.2) +
#'   declare_test(Z ~ cov1 + cov2 + cov3, model = lm_robust, model_summary = glance)
#'
#' diagnosis <- diagnose_design(
#'   design = balance_test_design,
#'   diagnosands = declare_diagnosands(false_positive_rate = mean(p.value <= 0.05), keep_defaults = FALSE)
#' )
#' 
#' # K-S test of distributional equality
#' 
#' ks_test <- function(data) {
#'   test <- with(data, ks.test(x = Y[Z == 1], y = Y[Z == 0]))
#'   data.frame(statistic = test$statistic, p.value = test$p.value)
#' }
#' 
#' distributional_equality_design <-
#'   declare_population(N = 100) + 
#'   declare_potential_outcomes(Y_Z_1 = rnorm(N), Y_Z_0 = rnorm(N, sd = 1.5)) + 
#'   declare_assignment(prob = 0.5) + 
#'   declare_reveal(Y, Z) + 
#'   declare_test(handler = tidy_test(ks_test), label = "ks-test")
#'   
#' diagnosis <- diagnose_design(
#'   design = distributional_equality_design,
#'   diagnosands = declare_diagnosands(select = power)
#' ) 
#' 
#' # Thanks to Jake Bowers for this example
#' 
#' library(coin) 
#' 
#' our_ttest <- function(data) {
#'   res <- coin::oneway_test(
#'     outcome ~ factor(Xclus),
#'     data = data,
#'     distribution = "asymptotic"
#'   )
#'   data.frame(p.value = pvalue(res)[[1]])
#' }
#' 
#' ttest_design <- 
#'   declare_population(N = 100, Xclus = rbinom(n = N, size = 1, prob = 0.2), outcome = 3 + rnorm(N)) +
#'   declare_test(handler = tidy_test(our_ttest), label = "t-test")
#'   
#' diagnosis <- diagnose_design(
#'   design = ttest_design,
#'   diagnosands = declare_diagnosands(false_positive_rate = mean(p.value <= 0.05), keep_defaults = FALSE)
#' )
#' 
declare_test <- declare_estimator
declare_tests <- declare_test

#' @details
#' \code{tidy_test} takes an untidy test function, and returns a tidy handler which accepts standard labeling options.
#'
#' @param fn A function that takes a data.frame as an argument and returns a data.frame with the test statistic (and other inferential statistics) and a label.
#' @rdname declare_test
#' @export
tidy_test <- tidy_estimator