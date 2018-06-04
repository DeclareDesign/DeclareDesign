
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
#' reshape_diagnosis(diagnosis)
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
  structure(object, class = c("summary.diagnosis", "data.frame"))
}

#' @export
print.summary.diagnosis <- function(x, ...) {
  n_sims <- unique(x$diagnosands_df$n_sims)
  cat(paste0("\nResearch design diagnosis", ifelse(length(n_sims) == 1, paste0(" based on ", n_sims, " simulations"), ""), "."))
  if (x$bootstrap_sims > 0) {
    cat(" Diagnosand estimates with bootstrapped standard errors (", x$bootstrap_sims, " replicates).", sep = "")
  }
  cat("\n\n")
  x <- reshape_diagnosis(x)
  class(x) <- "data.frame"
  print(x, row.names = FALSE)
  invisible(x)
}


#' Clean up DeclareDesign diagnosis object for printing
#'
#' If diagnosands are bootstrapped, se's are put in parenthese on a second line and rounded to \code{digits}.
#'
#' @param diagnosis An object from \code{diagnose_design}, either a diagnosand dataframe or a list containing a diagnosand dataframe
#' @param digits Number of digits.
#' @param select List of columns to include in output. Defaults to all.
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#' @export
#'
#' @examples
#' # library(DesignLibrary)
#' # diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3)
#' # reshape_diagnosis(diagnosis)
#' # reshape_diagnosis(diagnosis, select = c("Bias", "Power"))
reshape_diagnosis <- function(diagnosis, digits = 2, select = NULL) {
  
  diagnosands_df <- diagnosis$diagnosands
  
  # Reshape if there is bootstrapping
  diagnosand_columns <- diagnosis$diagnosand_names
  diagnosand_se_columns <- paste0("se(", diagnosis$diagnosand_names, ")")
  group_columns <- names(diagnosands_df)[!names(diagnosands_df) %in% c(diagnosand_columns, diagnosand_se_columns)]
  
  # Finish up if no bootstrapping
  if (is.null(diagnosis$bootstrap_replicates)) {
    return(diagnosands_df)
  }
  
  # Make diagnosand only df
  diagnosands_only_df <-
    diagnosands_df[, c(group_columns, diagnosand_columns), drop = FALSE]
  
  clean_values_df <-
    data.frame(lapply(diagnosands_only_df[, diagnosand_columns, drop = FALSE],
                      format_num, digits = digits), stringsAsFactors = FALSE)
  
  diagnosands_only_df <-
    cbind(diagnosands_only_df[, group_columns, drop = FALSE],
          data.frame(statistic = "Diagnosand Estimate", stringsAsFactors = FALSE),
          clean_values_df)
  
  names(diagnosands_only_df) <- c(group_columns, "statistic", diagnosand_columns)
  
  # Make se only df
  se_only_df <- diagnosands_df[, diagnosand_se_columns, drop = FALSE]
  se_only_df <- data.frame(lapply(se_only_df, add_parens, digits = digits), stringsAsFactors = FALSE)
  colnames(se_only_df) <- diagnosand_columns
  
  se_only_df <- cbind(diagnosands_only_df[, group_columns, drop = FALSE],
                      data.frame(statistic = "SE (bootstrapped)", stringsAsFactors = FALSE), se_only_df)
  
  # Merge
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")
  
  # Reorder rows
  sort_by_list <- colnames(return_df) %i% c("design_ID", "estimator_label", "coefficient", "estimand_label", "statistic")
  return_df <- return_df[do.call(order, as.list(return_df[,sort_by_list])), , drop = FALSE]
  
  # NA bootstrap rows
  
  return_df[return_df$statistic == "SE (bootstrapped)", sort_by_list] <- ""
  
  # Select columns
  if (!is.null(select)) {
    if (any(!(select %in% names(return_df))))
      stop(paste(
        "select argument must only include elements from: ",
        paste(c(group_columns, diagnosand_columns), collapse = ",")
      ))
    return_df <- return_df[, select]
  }
  
  # Make names nicer
  names(return_df) <-
    gsub("\\b(se[(]|sd |rmse|[[:alpha:]])",
         "\\U\\1",
         gsub("_", " ", names(return_df)),
         perl = TRUE)
  
  return(return_df)
  
}

