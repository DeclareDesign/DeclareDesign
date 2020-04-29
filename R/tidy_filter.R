#' Tidy Model Results and Filter to Relevant Coefficients
#'
#' @param fit 
#' @param term 
#'
#' @return
#' @export
#'
#' @examples
tidy_filter <- function(fit, term = FALSE) {
  
  if (hasS3Method("tidy", fit)) {
    tidy_df <- tidy(fit, conf.int = TRUE)
  } else {
    tidy_df <- try(tidy_try(fit, conf.int = TRUE), silent = TRUE)
    
    if(inherits(tidy_df, "try-error")){
      stop("We were unable to tidy the output of the function provided to 'model'. 
           It is possible that the broom package has a tidier for that object type. 
           If not, you can use a custom estimator to 'estimator_function'.
           See examples in ?declare_estimator")
    }
  }
  
  if (is.character(term)) {
    coefs_in_output <- term %in% tidy_df$term
    if (!all(coefs_in_output)) {
      stop(
        "Not all of the terms declared in your estimator are present in the model output, including ",
        paste(term[!coefs_in_output], collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    tidy_df <- tidy_df[tidy_df$term %in% term, , drop = FALSE]
  } else if (is.logical(term) && !term) {
    tidy_df <- tidy_df[which.max(tidy_df$term != "(Intercept)"), , drop = FALSE]
  }
  
  tidy_df
}

tidy_try <- function(x, conf.int = TRUE) {
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
hasS3Method <- function(f, obj) {
  for(i in class(obj)) {
    get_function <- try(getS3method(f, i), silent = TRUE)
    if(class(get_function) != "try-error" && is.function(get_function)) return(TRUE)
  }
  FALSE
}
