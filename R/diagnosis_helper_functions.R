
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
  cat("\nResearch design diagnosis\n\n")
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
#' @param col.names Allows user to provide names of columns for output. If NULL uses names from diagnosis object, if "default" uses names of default diagnosands.
#' @param n_text_fields Number of initial text fields in diagnosis that do not have associated standard errors.
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#' @export
#'
#' @examples
#' my_population <- declare_population(N = 50, noise = rnorm(N))
#'
#' my_potential_outcomes <-
#'   declare_potential_outcomes(Y_Z_0 = noise,
#'                              Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
#'
#' my_assignment <- declare_assignment(m = 25)
#'
#' pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "pate")
#'
#' pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = "test")
#'
#'
#' my_design <- declare_design(my_population,
#'                             my_potential_outcomes,
#'                             pate,
#'                             my_assignment,
#'                             pate_estimator)
#'
#' my_diagnosis <- diagnose_design(my_design)
#'
#' reshape_diagnosis(my_diagnosis)
#'
#'
reshape_diagnosis <- function(diagnosis, digits = 2){

  diagnosands_df <- diagnosis$diagnosands
  diagnosands_df_names <- names(diagnosands_df)

  group_names <- diagnosands_df_names[1:3]
  value_names <- diagnosands_df_names[-(1:3)]


  diagnosands_only_names <- value_names[which(substr(value_names, start = 1, stop = 3) != "se(")]
  diagnosands_only_df <- diagnosands_df[,c(group_names, diagnosands_only_names), drop = FALSE]

  diagnosands_only_df <- cbind(diagnosands_only_df[, group_names, drop = FALSE],
                               data.frame(statistic = "Diagnosand Estimate"),
                               data.frame(lapply(
                                 diagnosands_only_df[, diagnosands_only_names, drop = FALSE], format_num, digits = digits)))

  if(sum(grepl(value_names, pattern = "se\\(")) == 0) {
    return(diagnosands_only_df)
  }

  se_only_names <- value_names[which(substr(value_names, start = 1, stop = 3) == "se(")]
  se_only_df <- diagnosands_df[,c(group_names, se_only_names), drop = FALSE]

  se_only_names_new <- substr(se_only_names, start = 4, stop = nchar(se_only_names) - 1)
  colnames(se_only_df) <- c(group_names, se_col_names_new)

  se_only_df <- cbind(se_only_df[,group_names, drop = FALSE],
                      data.frame(statistic = "Bootstrapped SE"),
                      data.frame(lapply(se_only_df[, se_only_names_new, drop = FALSE],
                                        add_parens, digits = digits)))

  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df))
  return_df <- return_df[with(return_df, order(estimator_label, coefficient, estimand_label, statistic)),]
  return_df

}






