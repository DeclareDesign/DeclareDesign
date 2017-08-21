#' Get the diagnosands data frame from a diagnosis
#'
#' @param diagnosis A diagnosis object created by \code{\link{diagnose_design}}.
#'
#' @return a \code{data.frame} of the diagnosand values from a diagnosis
#'
#' @export
get_diagnosands <- function(diagnosis) {
  diagnosis$diagnosands
}

#' Get the simulations data frame from a diagnosis
#'
#' @param diagnosis A diagnosis object created by \code{\link{diagnose_design}}.
#'
#' @return a \code{data.frame} of the simulations from a diagnosis
#'
#' @export
get_simulations <- function(diagnosis) {
  diagnosis$simulations
}

#' @export
print.diagnosis <- function(x, ...) {
  print(summary(x))
  invisible(summary(x))
}

#' @export
summary.diagnosis <- function(object, ...) {
  structure(object$diagnosands,
            class = c("summary.diagnosis", "data.frame"))
}

#' @export
print.summary.diagnosis <- function(x, ...) {
  class(x) <- "data.frame"
  cat("\nResearch design diagnosis\n\n")
  print_diagnosis <- x
  names(x) <-
    gsub("(^|[[:space:]])([[:alpha:]])",
         "\\1\\U\\2",
         gsub("_", " ", names(x)),
         perl = TRUE)
  print(x, row.names = FALSE)
  cat("\n")
  invisible(x)
}
