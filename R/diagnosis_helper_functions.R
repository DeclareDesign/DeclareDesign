
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
# print.summary.diagnosis <- function(x, ...) {
#   class(x) <- "data.frame"
#   cat(paste0("\nResearch design diagnosis, based on ", attr(x, "sims"), " simulations and ", attr(x, "bootstrap"), " bootstrap draws.\n\n"))
#   print_diagnosis <- x
#   names(x) <-
#     gsub("\\b(se[(]|sd |rmse|[[:alpha:]])",
#          "\\U\\1",
#          gsub("_", " ", names(x)),
#          perl = TRUE)
#   print(x, row.names = FALSE)
#   cat("\n")
#   invisible(x)
# }

print.summary.diagnosis <- function(x, ...) {
  sims <- attr(x, "sims")
  if(length(sims) > 1) sims <- paste0("(", paste0(sims, collapse = ", "), ")")
  cat(paste0("\nResearch design diagnosis, based on ", sims, " simulations and ",
             1*attr(x, "bootstrap"), " bootstrap draws.\n\n"))
  x <- reshape_diagnosis(x, is.extracted = TRUE)
  class(x) <- "data.frame"
  print_diagnosis <- x
  print(x, row.names = FALSE)
  cat("\n")
  invisible(x)
}


#' Clean up DeclareDesign diagnosis object for printing
#'
#' If diagnosands are bootstrapped, se's are put in parenthese on a second line and rounded to \code{digits}.
#'
#' @param diagnosis An object from \code{diagnose_design}, either a diagnosand dataframe or a list containing a diagnosand dataframe
#' @param digits Number of digits.
#' @param is.extracted If TRUE diagnosis is the diagnosands dataframe; if FALSE diagnosis is a list containing the diagnosands data frame as its second object
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#' @export
#'
#' @examples
#' library(DesignLibrary)
#' # diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3)
#' # reshape_diagnosis(diagnosis)

reshape_diagnosis <- function(diagnosis, digits = 2, is.extracted = FALSE) {

  # Housekeeping
  diagnosands_df  <- diagnosis
  if(!is.extracted) diagnosands_df <- diagnosis$diagnosands

  # Make names nicer
  names(diagnosands_df) <-
    gsub("\\b(se[(]|sd |rmse|[[:alpha:]])",
         "\\U\\1",
         gsub("_", " ", names(diagnosands_df)),
         perl = TRUE)

  # Finish up if no bootstrapping
  bootstrapped <- attr(diagnosands_df, "bootstrap") > 0
  if(!bootstrapped) return(diagnosands_df)

  # Reshape if there is bootstrapping
  k <- ncol(diagnosands_df) - 2*attr(diagnosands_df, "n_diagosands")

  diagnosands_df_names <- names(diagnosands_df)
  group_names <- diagnosands_df_names[1:k]
  value_names <- diagnosands_df_names[-(1:k)]
  diagnosands_names <- value_names[seq(1, length(value_names)-1, 2)]
  se_names    <- value_names[seq(2, length(value_names), 2)]

  # Make diagnosand only df
  diagnosands_only_df <- diagnosands_df[,c(group_names, diagnosands_names), drop = FALSE]

  clean_values_df <- data.frame(lapply(diagnosands_only_df[, diagnosands_names, drop = FALSE],
                    format_num, digits = digits))

  diagnosands_only_df <- cbind( diagnosands_only_df[, group_names, drop = FALSE], clean_values_df)

  names(diagnosands_only_df) <- c(group_names, diagnosands_names)

  # Make se only df
  se_only_df <- diagnosands_df[, se_names, drop = FALSE]
  se_only_df <- data.frame(lapply(se_only_df,add_parens, digits = digits))
  colnames(se_only_df) <- diagnosands_names

  # Merge
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")

  # Reorder rows
  nrows <- nrow(diagnosands_only_df)
  return_df <- return_df[rep(1:nrows, each = 2) + rep(c(0,nrows), nrows), ]

  return(return_df)

}

