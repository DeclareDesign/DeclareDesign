
#' Explore your design diagnosis
#'
#' @param diagnosis A design diagnosis created by \code{\link{diagnose_design}}.
#'
#' @examples
#' my_population <- declare_population(N = 500, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = noise, Y_Z_1 = noise +
#'   rnorm(N, mean = 2, sd = 2))
#'
#' my_assignment <- declare_assignment()
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' my_reveal <- declare_reveal()
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_estimand,
#'                          my_assignment,
#'                          my_reveal,
#'                          my_estimator)
#'
#' \dontrun{
#' # using built-in defaults:
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' }
#'
#' # using a user-defined diagnosand
#' my_diagnosand <- declare_diagnosands(absolute_error = mean(abs(est - estimand)))
#'
#' \dontrun{
#' diagnosis <- diagnose_design(design, diagnosands = my_diagnosand)
#' diagnosis
#'
#' get_diagnosands(diagnosis)
#'
#' get_simulations(diagnosis)
#'
#' }
#'
#' @name diagnosis_helpers
NULL

#' @rdname diagnosis_helpers
#' @export
get_diagnosands <- function(diagnosis) {
  diagnosis$diagnosands
}

#' @rdname diagnosis_helpers
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
  cat(paste0("\nResearch design diagnosis, based on ", attr(x, "sims"), " simulations and ", attr(x, "bootstrap"), " bootstrap draws.\n\n"))
  print_diagnosis <- x
  names(x) <-
    gsub("\\b(se[(]|sd |rmse|[[:alpha:]])",
         "\\U\\1",
         gsub("_", " ", names(x)),
         perl = TRUE)
  print(x, row.names = FALSE)
  cat("\n")
  invisible(x)
}



#' Clean up DeclareDesign diagnosis object for printing
#'
#' If diagnosands are bootstrapped, se's are put in parenthese on a second line and rounded to \code{digits}.
#' Function uses presence of "se(" to identify bootrapped diagnoses; avoid errors by not using "se(" in naming of diagnosands.
#'
#' @param diagnosis An object from \code{declare_design}
#' @param digits Number of digits.
#' @param is.extracted If TRUE diagnosis is a dataframe; if FALSE diagnosis is a list containing the diagnosands data frame as its second object
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#' @export
#'
#' @examples
#' # diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3)
#' # reshape_diagnosis(diagnosis)
#' # reshape_diagnosis(diagnosis, col.names = 1:11)
#' # reshape_diagnosis(diagnosis, col.names = "default")
#' # diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3, bootstrap = 0)
#' # reshape_diagnosis(diagnosis, col.names = "default")

reshape_diagnosis <- function(diagnosis, digits = 2, is.extracted = FALSE
) {

  # Housekeeping
  bootstrapped <- attr(diagnosis, "bootstrap") > 0
  if(!is.extracted) diagnosis     <- diagnosis[[2]]

  if(!bootstrapped) return(diagnosis)

  n_text_fields <- min(which(grepl("se\\(", names(diagnosis)))) - 2
  D             <- as.matrix(diagnosis[,(n_text_fields+1):ncol(diagnosis)])
  rows          <- nrow(D)

    cols       <- ncol(D)/2
    out.width  <- cols+n_text_fields

    # Reformatting
    out <- matrix(NA, 2*rows, out.width)
    out[2*(1:rows)-1, (n_text_fields + 1):ncol(out)] <- round(D[,2*(1:cols)-1], digits)
    out[2*(1:rows),   (n_text_fields + 1):ncol(out)] <- paste0("(", round(D[,2*(1:cols)], digits), ")")

    out[2*(1:rows)-1, 1:n_text_fields] <- as.matrix(diagnosis)[, 1:n_text_fields]
    out[2*(1:rows), 1:n_text_fields] <- " "
    colnames(out) <- colnames(diagnosis[,c(1:n_text_fields, n_text_fields+2*(1:cols)-1)])

    return(out)
}

