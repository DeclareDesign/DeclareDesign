#' Declare test
#'
#' @description Declares an test which generates a test statistic and associated inferential statistics. 
#' 
#' Use of \code{declare_test} is identical to use of \code{\link{declare_estimator}}. Use \code{declare_test} for hypothesis testing with no specific inquiry in mind; use \code{declare_estimator} for hypothesis testing when you can link each estimate to an inquiry. For example, \code{declare_test} could be used for a K-S test of distributional equality and \code{declare_estimator} for a difference-in-means estimate of an average treatment effect.
#' 
#' See \code{\link{declare_estimator}} help for an explanation of how to use \code{model_handler}, which is used identically in both \code{declare_estimator} and \code{declare_test}. The main difference between \code{declare_estimator} and \code{declare_test} is that \code{declare_test} does not link with an explicit inquiry.
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
#'   declare_model(
#'     N = 100, 
#'     cov1 = rnorm(N), 
#'     cov2 = rnorm(N), 
#'     cov3 = rnorm(N)
#'   ) +
#'   declare_assignment(Z = complete_ra(N, prob = 0.2)) +
#'   declare_test(Z ~ cov1 + cov2 + cov3, model = lm_robust, model_summary = glance)
#'   
#' \dontrun{
#' diagnosis <- diagnose_design(
#'   design = balance_test_design,
#'   diagnosands = declare_diagnosands(
#'   false_positive_rate = mean(p.value <= 0.05))
#' )
#' }
#' 
#' # K-S test of distributional equality
#' 
#' ks_test <- function(data) {
#'   test <- with(data, ks.test(x = Y[Z == 1], y = Y[Z == 0]))
#'   data.frame(statistic = test$statistic, p.value = test$p.value)
#' }
#' 
#' distributional_equality_design <-
#'   declare_model(
#'     N = 100, 
#'     Y_Z_1 = rnorm(N), 
#'     Y_Z_0 = rnorm(N, sd = 1.5)
#'   ) + 
#'   declare_assignment(Z = complete_ra(N, prob = 0.5)) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
#'   declare_test(handler = label_test(ks_test), label = "ks-test")
#'   
#' \dontrun{
#' diagnosis <- diagnose_design(
#'   design = distributional_equality_design,
#'   diagnosands = declare_diagnosands(power = mean(p.value <= 0.05))
#' ) 
#' }
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
#'   declare_model(
#'     N = 100, 
#'     Xclus = rbinom(n = N, size = 1, prob = 0.2), 
#'     outcome = 3 + rnorm(N)) +
#'   declare_test(handler = label_test(our_ttest), label = "t-test")
#'   
#' \dontrun{
#' diagnosis <- diagnose_design(
#'   design = ttest_design,
#'   diagnosands = declare_diagnosands(
#'     false_positive_rate = mean(p.value <= 0.05))
#' )
#' }
#' 
#' @seealso See \code{\link{declare_estimator}} for documentation of the \code{model_handler} function.
#' 
declare_test <- 
  make_declarations(
    label_test(model_handler),
    step_type = "estimator",
    causal_type = "estimator",
    default_label = "test"
  )

declare_tests <- declare_test

#' @details
#' \code{label_test} takes a data-in-data out function to \code{fn}, and returns a data-in-data-out function that first runs the provided test function \code{fn} and then appends a label for the test.
#'
#' @param fn A function that takes a data.frame as an argument and returns a data.frame with test statistics as columns.
#' @rdname declare_test
#' @export
label_test <- function(fn) {
  if (!("data" %in% names(formals(fn)))) {
    stop("Must provide a `test_function` function with a data argument.")
  }
  
  f <- function(data, ..., inquiry = NULL, label) {
    
    calling_args <-
      names(match.call(expand.dots = FALSE)) %i% names(formals(fn))
    
    dots <- if ("..." %in% calling_args) {
      quos(...)
    } else {
      list()
    }
    
    calling_args <- setdiff(calling_args, c("", "data", "..."))
    
    for (e in calling_args) {
      dots[[e]] <-
        do.call(enquo, list(as.symbol(e))) # this *should* retrieve term names as quosure. IDK
    }
    
    ret <- eval_tidy(quo(fn(data, !!!dots)))
    
    ret <- data.frame(
      estimator = label,
      ret,
      stringsAsFactors = FALSE
    )
    
    ret
  }
  
  formals(f) <- formals(fn)
  if (!"label" %in% names(formals(f))) {
    formals(f)$label <- alist(a = )$a
  }
  
  attributes(f) <- attributes(fn)
  
  f
}

