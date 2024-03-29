#' Explore your design diagnosis
#'
#' @param diagnosis A design diagnosis created by \code{\link{diagnose_design}}.
#'
#' @examples
#' 
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' \dontrun{
#' # Diagnose design using default diagnosands
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' 
#' # Use get_diagnosands to explore diagnosands:
#' get_diagnosands(diagnosis)
#' 
#' # Use get_simulations to explore simulations
#' get_simulations(diagnosis)
#' 
#' # Exploring user-defined diagnosis your own diagnosands
#' my_diagnosands <-
#'   declare_diagnosands(median_bias = median(estimate - estimand),
#'                       absolute_error = mean(abs(estimate - estimand)))
#' 
#' diagnosis <- diagnose_design(design, diagnosands = my_diagnosands)
#' diagnosis
#' 
#' tidy(diagnosis)
#' 
#' reshape_diagnosis(diagnosis)
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
print.diagnosis <- function(x, digits = 2, select = NULL, exclude = NULL, ...) {
  print_summary_diagnosis(x, digits = digits, select = select, exclude = exclude)
}

#' @export
summary.diagnosis <- function(object, digits = 2, select = NULL, exclude = NULL, ...) {
  print_summary_diagnosis(object, digits = digits, select = select, exclude = exclude)
}

print_summary_diagnosis <- function(x, digits = 2, select = NULL, exclude = NULL, ...) {
  n_sims <- unique(x$diagnosands_df$n_sims)
  cat(paste0("\nResearch design diagnosis", ifelse(length(n_sims) == 1, paste0(" based on ", n_sims, " simulations"), ""), ". Diagnosis completed in ", round(as.numeric(x$duration)), " ", attr(x$duration, "units"), "."))
  if (x$bootstrap_sims > 0) {
    cat(" Diagnosand estimates with bootstrapped standard errors in parentheses (", x$bootstrap_sims, " replicates).", sep = "")
  }
  cat("\n\n")
  x <- reshape_diagnosis(x, digits = digits, select = select, exclude = exclude)
  class(x) <- "data.frame"
  print(x, row.names = FALSE)
  invisible(x)
}


#' Clean up a diagnosis object for printing
#'
#' Take a diagnosis object and returns a pretty output table. If diagnosands are bootstrapped, se's are put in parentheses on a second line and rounded to \code{digits}.
#'
#' @param diagnosis A diagnosis object generated by \code{diagnose_design}. 
#' @param digits Number of digits.
#' @param select List of columns to include in output. Defaults to all.
#' @param exclude Set of columns to exclude from output. Defaults to none.
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#'
#' @examples
#' 
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' \dontrun{
#' # Diagnose design using default diagnosands
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' 
#' # Return diagnosis output table
#' reshape_diagnosis(diagnosis)
#' 
#' # Return table with subset of diagnosands
#' reshape_diagnosis(diagnosis, select = c("Bias", "Power"))
#' 
#' # With user-defined diagnosands
#' my_diagnosands <-
#'   declare_diagnosands(median_bias = median(estimate - estimand),
#'                       absolute_error = mean(abs(estimate - estimand)))
#' 
#' diagnosis <- diagnose_design(design, diagnosands = my_diagnosands)
#' diagnosis
#' 
#' reshape_diagnosis(diagnosis)
#' 
#' reshape_diagnosis(diagnosis, select = "Absolute Error")
#' 
#' # Alternative: Use tidy to produce data.frame with results of 
#' # diagnosis including bootstrapped standard errors and 
#' # confidence intervals for each diagnosand
#' diagnosis_df <- tidy(diagnosis)
#' diagnosis_df
#'
#' }
#' @export
reshape_diagnosis <- function(diagnosis, digits = 2, select = NULL, exclude = NULL) {
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

  # Reorder rows
  sort_by_list <- diagnosis$group_by_set %icn% return_df

  return_df <- return_df[do.call(order, as.list(return_df[, sort_by_list])), , drop = FALSE]

  # blank cells for SE rows
  for(i in c(parameter_names, "design")) {
    levels(return_df[[i]]) <- c(levels(return_df[[i]]), "")
  }

  if(!is.null(return_df$estimator)) 
  return_df$estimator <- as.character(return_df$estimator)
  
  for(j in c(sort_by_list, parameter_names)) if(is.factor(return_df[[j]]) && !"" %in% levels(return_df[[j]]))
    return_df[[j]] <- factor(return_df[[j]], levels = c(levels(return_df[[j]]), ""))
  
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
  
  if(!is.null(exclude))
    return_df <- return_df[, names(return_df)[!(names(return_df) %in% exclude)]] 
  
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
  return_df$design <- factor(return_df$design)
  return_df$design <- factor(return_df$design, levels = c(levels(return_df$design), ""))

  return(return_df)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy diagnosis 
#'
#' @param x A diagnosis object generated by \code{diagnose_design}. 
#' @param conf.int  Logical indicating whether or not to include a
#'   confidence interval in the tidied output. Defaults to ‘TRUE’.
#' @param conf.level  The confidence level to use for the confidence
#'   interval if ‘conf.int = TRUE’. Must be strictly greater than 0 and less
#'   than 1. Defaults to 0.95, which corresponds to a 95 percent confidence
#'   interval.
#' @param ... extra arguments (not used)
#'
#' @return A data.frame with columns for diagnosand names, estimated diagnosand values, 
#' bootstrapped standard errors and confidence intervals
#' 
#' @export
#'
#' @examples
#' 
#' effect_size <- 0.1
#' design <-
#'   declare_model(
#'     N = 100,
#'     U = rnorm(N),
#'     X = rnorm(N),
#'     potential_outcomes(Y ~ effect_size * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = complete_ra(N)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE", label = "unadjusted") + 
#'   declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "adjusted")
#' 
#' diagnosis <- diagnose_design(design, sims = 100)
#' 
#' tidy(diagnosis)
#' 
#' @importFrom stats reshape
tidy.diagnosis <- function(x,
                           conf.int = TRUE,
                           conf.level = 0.95,
                           ...) {
  
  alpha <- 1 - conf.level
  if(is.null(alpha) || is.na(alpha)) {
    stop("Please provide a conf.level.")
  }
  
  diagnosand_columns <- x$diagnosand_names
  
  diagnosands_df <- get_diagnosands(x)
  
  parameter_names <- names(x$parameters_df)[-1]
  
  group_by_set <- x$group_by_set
  
  diagnosands_df <-
    cbind(data.frame(lapply(diagnosands_df[, c(group_by_set, parameter_names), drop = FALSE], function(x) if(any(is.na(x))) addNA(x) else x)),
          diagnosands_df[, names(diagnosands_df)[!names(diagnosands_df) %in% c(group_by_set, parameter_names)], drop = FALSE])
  
  diagnosands_df_long <- 
    reshape(
    diagnosands_df[,c(group_by_set, parameter_names, diagnosand_columns)], 
    idvar = c(group_by_set, parameter_names), 
    times = diagnosand_columns, 
    timevar = "diagnosand",
    varying = list(diagnosand_columns), 
    direction = "long",
    v.names = "estimate",
    new.row.names = NULL
  )
  
  if (is.data.frame(x$bootstrap_replicates)) {
    diagnosands_se_columns <- paste0("se(", diagnosand_columns, ")")

    diagnosands_se_df_long <- reshape(
      diagnosands_df[,c(group_by_set, parameter_names, diagnosands_se_columns)], 
      idvar = c(group_by_set, parameter_names), 
      times = diagnosands_se_columns, 
      timevar = "diagnosand",
      varying = list(diagnosands_se_columns), 
      direction = "long",
      v.names = "std.error",
      new.row.names = NULL
    )
    
    diagnosands_df_long$std.error <- diagnosands_se_df_long[, "std.error"]
    
  }
  
  if (is.data.frame(x$bootstrap_replicates) && conf.int == TRUE) {
    
    diagnosand_replicates <- x$bootstrap_replicates
    
    group_by_list <- diagnosand_replicates[, c(group_by_set, parameter_names), drop = FALSE]
    
    labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE, sep = "uNiquEsEp")
    labels_df <- lapply(labels_df, head, n = 1)
    
    use_vars <- setdiff(names(diagnosand_replicates), c(group_by_set, parameter_names, "bootstrap_id"))
    diagnosands_summary_df <- split(diagnosand_replicates[use_vars], lapply(group_by_list, addNA), drop = TRUE, sep = "uNiquEsEp")

    diagnosands_conf_low_list <- lapply(diagnosands_summary_df, function(data) lapply(data[diagnosand_columns], quantile_NAsafe, alpha/2))
    diagnosands_conf_high_list <- lapply(diagnosands_summary_df, function(data) lapply(data[diagnosand_columns], quantile_NAsafe, 1-alpha/2))
    
    diagnosands_conf_low_df <- unlist2(diagnosands_conf_low_list, sep = "uNiquEsEp")
    diagnosands_conf_high_df <- unlist2(diagnosands_conf_high_list, sep = "uNiquEsEp")
    
    make_df <- function(vec, var) {
      nm <- names(vec)
      val <- vec
      nm_split <- strsplit(nm, split = "uNiquEsEp", fixed = TRUE)
      nm_df <- t(sapply(nm_split, c))
      colnames(nm_df) <- c(group_by_set, parameter_names, "diagnosand")
      df <- data.frame(nm_df)
      df[, var] <- val
      df
    }
    
    diagnosands_conf_low_df <- make_df(diagnosands_conf_low_df, "conf.low")
    diagnosands_conf_high_df <- make_df(diagnosands_conf_high_df, "conf.high")
    
    diagnosands_df_long <- merge(diagnosands_df_long, diagnosands_conf_low_df, by = c(group_by_set, parameter_names, "diagnosand"), all = TRUE)
    diagnosands_df_long <- merge(diagnosands_df_long, diagnosands_conf_high_df, by = c(group_by_set, parameter_names, "diagnosand"), all = TRUE)
    
  } 
  
  sort_by_list <- c(group_by_set, parameter_names, "diagnosand")
  
  # ordering diagnosands in the order defined by the user (factor then back to character)
  diagnosands_df_long$diagnosand <- factor(diagnosands_df_long$diagnosand, levels = diagnosand_columns)
  diagnosands_df_long <- diagnosands_df_long[do.call(order, as.list(diagnosands_df_long[sort_by_list])), , drop = FALSE]
  diagnosands_df_long$diagnosand <- as.character(diagnosands_df_long$diagnosand)
  
  rownames(diagnosands_df_long) <- NULL
  
  diagnosands_df_long
  
}

quantile_NAsafe <- function(x, ...) {
  if(anyNA(x)) {
    NA 
  } else {
    ret <- quantile(x, ...)
    names(ret) <- NULL
    ret
  }
}

# from https://stackoverflow.com/questions/37513593/r-name-concatenation-separator-in-unlist-flattening-list-of-lists
unlist2 <- function(x, sep = "_") {
  # save top names
  top_names <- names(x)
  x <- unname(x)
  
  # flatten
  x2 <- unlist(x)
  
  # add prefix
  # determine how many prefixes to add of each
  lengths_top <- sapply(x, length)
  prefixes <- rep(top_names, times = lengths_top)
  names(x2) <- paste0(prefixes, sep, names(x2))
  
  x2
}

