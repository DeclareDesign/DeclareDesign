#' Declare test
#'
#' @description Declares an test which generates a test statistic and associated inferential statistics
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @export
#'
#' @return A function that accepts a data.frame as an argument and returns a data.frame containing the value of the test statistic and other inferential statistics.
#'
#' @examples
#' 
#' # balance_test_design <- 
#' #   declare_population(N = 100, cov1 = rnorm(N), cov2 = rnorm(N), cov3 = rnorm(N)) +
#' #   declare_assignment(prob = 0.2) + 
#' #   declare_test(Z ~ cov1 + cov2 + cov3, model = lm_robust, post_estimation = glance)
#' #   
#' # diagnosis <- diagnose_design(
#' #   design = balance_test_design,
#' #   diagnosands = declare_diagnosands(false_positive_rate = mean(p.value <= 0.05), keep_defaults = FALSE)
#' # )
#' 
#' # Thanks to Jake Bowers for this example
#' 
#' our_ttest <- function(data) {
#'   require(coin)
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
