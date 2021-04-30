#' Compare Diagnoses
#'
#' Diagnose and compare designs.
#'
#' @param design1 A design or a diagnosis.
#' @param design2 A design or a diagnosis.
#' @param sims The number of simulations, defaulting to 1000. \code{sims} may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}. Used for both designs.
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{1000}. Set to \code{FALSE} to turn off bootstrapping. Used for both designs. Must be greater or equal to 100.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}.
#' @param alpha The significance level, 0.05 by default.
#'
#' @return A list with a \code{data.frame} of compared diagnoses and both diagnoses.
#'
#' @importFrom stats quantile
#'
#' @details
#'
#' The function \code{compare_diagnoses} runs a many-to-many merge matching by \code{inquiry_label} and \code{term} (if present). If  \code{merge_by_estimator} equals \code{TRUE}, \code{estimator_label} is also included in the merging condition. Any diagnosand that is not included in both designs will be dropped from the merge.
#'
#' @examples
#' design_a <- declare_model(N  = 100, 
#'                           U = rnorm(N),
#'                           Y_Z_0 = U,
#'                           Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)) +
#' declare_assignment(Z = complete_ra(N, prob = 0.5), legacy = FALSE) +
#' declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#' declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#' declare_estimator(Y ~ Z, inquiry = "ATE")
#'
#' design_b <- replace_step(
#'   design_a, step = "assignment", 
#'   declare_assignment(Z = complete_ra(N, prob = 0.3), legacy = FALSE) )
#'
#' comparison <- compare_diagnoses(design_a, design_b, sims = 40)
#'
#' @export
compare_diagnoses <- function(design1,
                              design2,
                              sims = 500,
                              bootstrap_sims = 100,
                              merge_by_estimator = TRUE,
                              alpha = 0.05) {
  if (bootstrap_sims <= 99) {
    stop("Diagnoses must have at least 100 bootstrap simulations.")
  }
  
  if (!inherits(design1, "design") | !inherits(design2, "design")) {
    stop("design1 and design2 must be of class design")
  }
  
  diagnosis1 <- diagnose_design(design1,
                                sims = sims,
                                bootstrap_sims =  bootstrap_sims)
  
  diagnosis2 <- diagnose_design(design2,
                                sims = sims,
                                bootstrap_sims = bootstrap_sims)
  
  compare_diagnoses_internal(diagnosis1,
                             diagnosis2,
                             sims,
                             bootstrap_sims,
                             merge_by_estimator,
                             alpha)
  
}

compare_diagnoses_internal <-
  function(diagnosis1,
           diagnosis2,
           sims,
           bootstrap_sims,
           merge_by_estimator,
           alpha) {
    
    diagnosands <- intersect(diagnosis1$diagnosand_names, diagnosis2$diagnosand_names)
    
    # Get merge_set 
    # merge_by_set, used to merge diagnosands_df, must at least contain inquiry_label
    # At its largest possible cardinality, merge_by_set contains c('inquiry_label', 'estimator_label', 'term')
    group_by_set1 <- diagnosis1$group_by_set
    group_by_set2 <- diagnosis2$group_by_set
    
    inquiry_in_set  <-
      "inquiry_label" %in% group_by_set1 &
      "inquiry_label" %in% group_by_set2
    
    estimator_in_set <-
      "estimator_label" %in% group_by_set1 &
      "estimator_label" %in% group_by_set2
    
    merge_by_set <- NULL
    
    # Stop if neither inquiry_label nor estimator_label are included in both diagnoses
    if ((inquiry_in_set + estimator_in_set) < 1) {
      stop("Both diagnosands_df must contain at least inquiry_label or estimator_label")
    }
    
    # Start adding names to the set of columns used for merging
    if (inquiry_in_set) {
      merge_by_set <- c("inquiry_label")
    }
    
    if (merge_by_estimator) {
      if (!estimator_in_set) {
        warning(
          "Estimator label not used for merging. At least one of the designs doesn't contain estimator_label."
        )
      } else {
        merge_by_set <- c(merge_by_set, "estimator_label")
      }
    }
    
    if ("term" %in% group_by_set1 & "term" %in% group_by_set2) {
      merge_by_set <- c(merge_by_set, "term")
    }
    
    if (estimator_in_set & (!"estimator_label" %in% merge_by_set)) {
      group_by_set <- c(merge_by_set, "estimator_label")
    } else{
      group_by_set <- merge_by_set
    }
    
    # Merge diagnoses
    # stop if no matching found
    # drop all columns that don't match
    comparison_df <- merge(
      diagnosis1$diagnosands_df,
      diagnosis2$diagnosands_df,
      by = merge_by_set,
      suffixes = c("_1", "_2"),
      stringsAsFactors = FALSE
    )
    
    if (nrow(comparison_df) == 0) {
      stop("Can't merge the two diagnoses, because they do not have labels in common.")
    }
    
    # Drop cols without matching pair
    c_names <- colnames(comparison_df)
    filter_se <- grepl("^se", c_names)
    suffix <- c("_1", "_2")
    keepcols <-
      grepl("[[:digit:]]+$", c_names) |
      grepl("label+$", c_names) | grepl("term", c_names) | filter_se
    comparison_df <- comparison_df[, keepcols, drop = FALSE]
    
    # Divide comparison_df into groups defined by set= c(inquiry_label, estimator_label, term)
    c_names <- colnames(comparison_df)
    set <-
      grepl("^inquiry_label", c_names) |
      grepl("^estimator_label", c_names) | grepl("^term", c_names)
    set <- unique(comparison_df[, c_names[set], drop = FALSE])
    comparison_list <-
      split(comparison_df, lapply(set, addNA), drop = TRUE)
    
    # Function to split bootstrap into groups defined by merge by set
    split_bootstrap <- function(diagnosis, set) {
      bootstrap_df <- diagnosis$bootstrap_replicates
      group_by_list <- bootstrap_df[, set, drop = FALSE]
      split(bootstrap_df, lapply(group_by_list, addNA), drop = TRUE)
    }
    
    bootstrap_df1 <- split_bootstrap(diagnosis1, group_by_set)
    bootstrap_df2 <- split_bootstrap(diagnosis2, group_by_set)
    
    # Function to create output.
    # For each combination of inquiry_label, estimator_label and term in comparison_list
    # take the mean and the se of each estimated diagnosand "d" from comparison_list
    # and unite with
    # the mean difference computed as mean(bootstrap_df2[,"d"] - bootstrap_df1[, "d"]
    # se of the difference computed as sd(bootstrap_df2[,"d"] - bootstrap_df1[, "d"])
    # p.value computed as the proportion of times that the difference is on opposite side of zero from mean difference
    create_difference_df <- function(label1,
                                     label2,
                                     comparison_label,
                                     df1,
                                     df2,
                                     comparison,
                                     diagnosands_list) {
      # Estimated diagnosands and se(diagnosands) from each design
      d1 <- df1[[label1]]
      d2 <- df2[[label2]]
      comp <- comparison[[comparison_label]]
      c_names <- colnames(comp)
      set <-
        grepl("^inquiry_label", c_names) |
        grepl("^estimator_label", c_names) | grepl("^term", c_names)
      select_cols <- c_names[grepl("^design_label", c_names)]
      select_cols <- c(select_cols, c_names[set])
      identifiers <- comp[, select_cols, drop = FALSE]
      
      # Compute difference stats
      difference <-
        do.call(rbind, lapply(diagnosands_list, function(diagnosand) {
          diagnosand1 <- comp[, paste0(diagnosand, "_1")]
          diagnosand2 <- comp[, paste0(diagnosand, "_2")]
          se1 <- comp[, paste0("se(", diagnosand, ")_1")]
          se2 <- comp[, paste0("se(", diagnosand, ")_2")]
          diff <- d2[, diagnosand] - d1[, diagnosand]
          mu <- mean(diff, na.rm = TRUE)
          conf.low  <- quantile(diff, alpha / 2, na.rm = TRUE)
          conf.high <- quantile(diff, 1 - (alpha / 2) , na.rm = TRUE)
          
          # Combine
          out <- data.frame(
            diagnosand = diagnosand,
            mean_1 = diagnosand1,
            mean_2 = diagnosand2,
            mean_difference = mu,
            se_1 = se1,
            se_2 = se2,
            se_difference = sd(diff),
            conf.low  = conf.low,
            conf.high = conf.high,
            sims = sims,
            bootstrap_sims = bootstrap_sims,
            stringsAsFactors = FALSE
          )
          
        }))
      
      suppressWarnings(cbind(identifiers, difference))
      
    }
    
    comparison_labels  <- names(comparison_list)
    
    if (merge_by_estimator) {
      c_labels <- list(label1 = comparison_labels,
                       label2 = comparison_labels,
                       comparison_label = comparison_labels)
      
    } else {
      
      # Bit needed to do many-to-many comparisons of estimators keeping e.g., inquiry_label and term fixed (assigned to c_labels)
      # extract labels that were used for merging
      # find the levels in the split bootstraps_df that start with each of those labels
      # combine all possible levels that share the same e.g inquiry_label and term  (assigned to c_labels)
      k <- length(merge_by_set)
      c_labels <- unique(lapply(comparison_labels, function(x) {
        x <- unlist(strsplit(x, "." , fixed = TRUE))
        paste0(x[1:k], collapse = ".")
      }))
      
      label1 <-  names(bootstrap_df1)
      label2 <-  names(bootstrap_df2)
      c_labels <- lapply(c_labels, function(l) {
        l1 <- startsWith(label1, l)
        l2 <- startsWith(label2, l)
        cl <- startsWith(comparison_labels, l)
        data.frame(
          expand.grid(label1 = label1[l1], label2 = label2[l2]),
          comparison_label = comparison_labels[cl],
          stringsAsFactors = FALSE
        )
      })
      
      c_labels <- rbind_disjoint(c_labels, infill = "")
      
    }
    
    return_df <- mapply(
      create_difference_df,
      label1 = c_labels$label1,
      label2 = c_labels$label2,
      comparison_label =  c_labels$comparison_label,
      SIMPLIFY = FALSE,
      MoreArgs = list(
        df1 = bootstrap_df1,
        df2 = bootstrap_df2,
        comparison = comparison_list,
        diagnosands_list = diagnosands
      )
    )
    
    return_df <- rbind_disjoint(return_df, infill = "")
    
    return_list <- list(
      compared_diagnoses_df = return_df,
      diagnosis1 = diagnosis1,
      diagnosis2 = diagnosis2
    )
    
    attr(return_list, "alpha") <- alpha
    class(return_list) <- "compared_diagnoses"
    
    return_list
    
  }

#' @export
print.compared_diagnoses <- function(x, ...) {
  print(summary(x))
  invisible(x)
}

#' @export
summary.compared_diagnoses <- function(object, ...) {
  structure(object, class = c("summary.compared_diagnoses", "data.frame"))
}

#' @export
print.summary.compared_diagnoses <- function(x, conf.int = FALSE, ...) {
  
  bootstrap_sims <- x$diagnosis1$bootstrap_sims
  sims <- nrow(x$diagnosis1$simulations_df)
  compared_diagnoses_df <- x[["compared_diagnoses_df"]]
  alpha <- attr(x, "alpha")
  column_names <- colnames(compared_diagnoses_df)
  identifiers <-
    column_names %in% c("inquiry_label", "term") |
    grepl("^estimator_label", column_names)
  identifiers_columns <- column_names[identifiers]
  conf.low  <- compared_diagnoses_df$conf.low
  conf.high <- compared_diagnoses_df$conf.high
  flag <- rep("", length(conf.low))
  flag[conf.low  > 0 |  conf.high < 0] <- "*"
  
  # Make diagnosand only df
  clean_comparison_df <-
    compared_diagnoses_df[, c(identifiers_columns,
                              "diagnosand",
                              "mean_1",
                              "mean_2",
                              "mean_difference"), drop = FALSE]
  
  clean_values_df <-
    data.frame(lapply(
      clean_comparison_df[, c("mean_1", "mean_2", "mean_difference"), drop = FALSE], format_num,
      digits = 2),
      stringsAsFactors = FALSE)
  
  clean_values_df$mean_difference <-
    paste0(clean_values_df$mean_difference, flag)
  
  diagnosands_only_df <-
    cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], clean_values_df)
  
  colnames(diagnosands_only_df)[colnames(diagnosands_only_df) %in% c("mean_1", "mean_2", "mean_difference")] <-
    c("design1", "design2", "difference")
  
  # Make se only df
  se_only_df <-
    compared_diagnoses_df[, grepl("^se", colnames(compared_diagnoses_df)), drop = FALSE]
  
  se_only_df <- data.frame(
    lapply(se_only_df, function(se) {
      paste0("(", format_num(se, digits = 2), ")")
    }),
    stringsAsFactors = FALSE)
  
  se_only_df <-
    cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], se_only_df)
  
  colnames(se_only_df) <- colnames(diagnosands_only_df)
  
  # Combine  mean and ses of diagnosands and difference
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")
  return_df <- return_df[do.call(order, as.list(return_df[, c(identifiers_columns, "diagnosand")])), , drop = FALSE]
  
  return_df[(1:nrow(return_df)) %% 2 == 0, c(identifiers_columns, "diagnosand")] <- ""
  
  # Print and return
  cat(paste0(
    "\nComparison of research designs diagnoses based on ",
    sims,
    " simulations."
  ))
  
  cat(
    paste0(
      " Diagnosand estimates with bootstrapped standard errors in parentheses (",
      bootstrap_sims,
      " replicates)."
    )
  )
  
  cat("\n\n")
  print(return_df, row.names = FALSE)
  cat(
    paste0(
      "\nNote: * indicates whether the ",
      (1 - alpha) * 100 ,
      "% bootstrap confidence interval of the difference in diagnosands excludes zero "
    )
  )
  invisible(return_df)
}
