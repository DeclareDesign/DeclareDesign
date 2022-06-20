#' Tidy Model Results and Filter to Relevant Coefficients
#' 
#' Tidy function that returns a tidy data.frame of model results and allows filtering to relevant coefficients. The function will attempt to tidy model objects even when they do not have a tidy method available. For best results, first load the broom package via \code{library(broom)}.
#'
#' @param fit A model fit, as returned by a modeling function like lm, glm, or estimatr::lm_robust.
#' @param term A character vector of the terms that represent quantities of interest, i.e., "Z". If FALSE, return the first non-intercept term; if TRUE return all terms.
#'
#' @return A data.frame with coefficient estimates and associated statistics.
#' 
#' @export
#' 
#' @examples
#' 
#' fit <- lm(mpg ~ hp + disp + cyl, data = mtcars)
#' 
#' tidy_try(fit)
#' 
tidy_try <- function(fit, term = FALSE) {
  
  if (hasS3Method("tidy", fit)) {
    tidy_df <- tidy(fit, conf.int = TRUE)
  } else {
    tidy_df <- try(tidy_try_internal(fit, conf.int = TRUE), silent = TRUE)
    
    if(inherits(tidy_df, "try-error")){
      stop("We were unable to tidy the output of the function provided to 'model'. 
           It is possible that the broom package has a tidier for that object type. 
           If not, you can use a custom estimator to 'estimator_function'.
           See examples in ?declare_estimator")
    }
  }
  tidy_df
}

tidy_try_internal <- function(x, conf.int = TRUE) {
  # TODO: error checking -- are column names named as we expect
  
  val <- try({
    summ <- coef(summary(x))
    
    if(conf.int == TRUE) {
      ci <- suppressMessages(as.data.frame(confint(x)))
      tidy_df <- data.frame(term = rownames(summ), summ, ci, stringsAsFactors = FALSE, row.names = NULL)
      colnames(tidy_df) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    } else {
      tidy_df <- data.frame(term = rownames(summ), summ, ci, stringsAsFactors = FALSE, row.names = NULL)
      colnames(tidy_df) <- c("term", "estimate", "std.error", "statistic", "p.value")
    }
    
  }, silent = TRUE)
  
  if(inherits(val, "try-error")){
    stop("The default tidy method for the model fit of class ", class(x), " failed. You may try installing and loading the broom package, or you can write your own tidy.", class(x), " method.", call. = FALSE)
  }
  
  tidy_df
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom utils getS3method
#' @importFrom methods extends
hasS3Method <- function(f, obj) {
  o_classes <- if(isS4(obj)) extends(class(obj)) else class(obj)
  for(i in o_classes) {
    get_function <- try(getS3method(f, i), silent = TRUE)
    if(!inherits(get_function, "try-error") && is.function(get_function)) return(TRUE)
  }
  FALSE
}
