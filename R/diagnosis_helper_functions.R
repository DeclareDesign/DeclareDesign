
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
#' design <- my_population +
#'   my_potential_outcomes +
#'   my_estimand +
#'   my_assignment +
#'   my_reveal +
#'   my_estimator
#'
#' \dontrun{
#' # using built-in defaults:
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' }
#'
#' # using a user-defined diagnosand
#' my_diagnosand <- declare_diagnosands(absolute_error = mean(abs(estimate - estimand)))
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
    cat(" Diagnosand estimates with bootstrapped standard errors in parentheses (", x$bootstrap_sims, " replicates).", sep = "")
  }
  cat("\n\n")
  x <- reshape_diagnosis(x)
  class(x) <- "data.frame"
  print(x, row.names = FALSE)
  invisible(x)
}


#' Clean up a diagnosis object for printing
#'
#' If diagnosands are bootstrapped, se's are put in parentheses on a second line and rounded to \code{digits}.
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
  diagnosand_columns <- diagnosis$diagnosand_names

  diagnosands_df <- diagnosis$diagnosands

  parameter_names <- names(diagnosis$parameters_df)[-1]

  if (is.data.frame(diagnosis$bootstrap_replicates)) {
    diagnosand_se_columns <- paste0("se(", diagnosis$diagnosand_names, ")")
    group_columns <- setdiff(names(diagnosands_df), c(diagnosand_columns, diagnosand_se_columns))
    return_df <- clean_bootstrap_df(
      diagnosis, digits, diagnosand_columns,
      diagnosand_se_columns, group_columns,
      parameter_names, sort_by_list
    )
  } else {
    group_columns <- setdiff(names(diagnosands_df), diagnosand_columns)
    return_df <- diagnosands_df
    
    return_df[diagnosand_columns] <- lapply(return_df[diagnosand_columns], 
                                            format_num, digits = digits)
  }

  sort_by_list <- diagnosis$group_by_set %i% colnames(return_df)

  # Reorder rows
  sort_by_list <- diagnosis$group_by_set %icn% return_df

  return_df <- return_df[do.call(order, as.list(return_df[, sort_by_list])), , drop = FALSE]

  # blank cells for SE rows
  levels(return_df$design_label) <- c(levels(return_df$design_label), "")
  return_df[return_df$statistic == "SE", c(sort_by_list, parameter_names, "n_sims")] <- ""
  return_df$statistic <- NULL

  # Make names nicer
  make_nice_names <- function(x) {
    gsub("\\b(se[(]|sd |rmse|[[:alpha:]])",
      "\\U\\1",
      gsub("_", " ", x),
      perl = TRUE
    )
  }

  names_to_change <- setdiff(names(return_df), parameter_names)
  names(return_df)[names(return_df) %in% names_to_change] <- make_nice_names(names_to_change)

  # Select columns
  if (!is.null(select)) {
    available_to_select <- make_nice_names(c(group_columns, diagnosand_columns))
    if (!all(select %in% available_to_select)) {
      stop(paste(
        "select argument must only include elements from: ",
        paste(available_to_select, collapse = ", ")
      ))
    }

    return_df <- return_df[, c(make_nice_names(c(sort_by_list, "n_sims")), select), drop = FALSE]
  }

  rownames(return_df) <- NULL
  return(return_df)
}

clean_bootstrap_df <- function(diagnosis, digits, diagnosand_columns,
                               diagnosand_se_columns, group_columns,
                               parameter_names, sort_by_list) {
  diagnosands_df <- diagnosis$diagnosands

  # Make diagnosand only df
  diagnosands_only_df <-
    diagnosands_df[, c(group_columns, diagnosand_columns), drop = FALSE]

  clean_values_df <-
    data.frame(lapply(diagnosands_only_df[, diagnosand_columns, drop = FALSE],
      format_num,
      digits = digits
    ), stringsAsFactors = FALSE)

  diagnosands_only_df <-
    cbind(
      diagnosands_only_df[, group_columns, drop = FALSE],
      data.frame(statistic = "Estimate", stringsAsFactors = FALSE),
      clean_values_df
    )

  names(diagnosands_only_df) <- c(group_columns, "statistic", diagnosand_columns)

  # Make se only df
  se_only_df <- diagnosands_df[, diagnosand_se_columns, drop = FALSE]
  se_only_df <- data.frame(lapply(se_only_df, add_parens, digits = digits), stringsAsFactors = FALSE)
  colnames(se_only_df) <- diagnosand_columns

  se_only_df <- cbind(
    diagnosands_only_df[, group_columns, drop = FALSE],
    data.frame(statistic = "SE", stringsAsFactors = FALSE), se_only_df
  )

  # Merge
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")

  # NA bootstrap rows
  return_df$design_label <- factor(return_df$design_label, levels = c(levels(return_df$design_label), ""))

  return(return_df)
}
