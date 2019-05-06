#' Compare Diagnoses
#'
#' Diagnose and compare designs.
#'
#' @param design1 A design or a diagnosis.
#' @param design2 A design or a diagnosis.
#' @param sims The number of simulations, defaulting to 1000. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}. Used for both designs.
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{1000}. Set to FALSE to turn off bootstrapping. Used for both designs. Must be greater or equal to 100.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}
#' @return A list with a data frame of compared diagnoses and both diagnoses.
#'
#' @details
#' 
#' The function \code{compare_diagnoses()} runs a many-to-many merge matching by \code{estimand_label} and \code{term} (if present). If  \code{merge_by_estimator} equals \code{TRUE}, \code{estimator_label} is also included in the merging condition. Any diagnosand that is not included in both designs will be dropped from the merge.
#' 
#' The data frame of compared diagnoses has a column, \code{in_interval}, that indicates if two given diagnosands diverge statistically. 
#' i.e. if a diagnosand from \code{design2} is not contained in the 95\% bootstrap confidence interval of their equivalent diagnosand from \code{design1}. The column \code{in_interval} equals zero for divergent diangnosands and one otherwise..
#'
#' @examples
#' design_a <- declare_population(N = 100, u = rnorm(N), X = runif(N, 0, 2)) +
#' 
#' declare_potential_outcomes(
#'   Y_Z_0 = u, 
#'   Y_Z_1 = u + rnorm(N, mean = 2, sd = 2)) +
#' declare_assignment() + 
#'
#' declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), label = "ATE") +
#' 
#' declare_reveal() +
#' 
#' declare_estimator(Y ~ Z, estimand = "ATE", label = "est1")
#' 
#' design_b <- replace_step(design_a, step = "assignment", declare_assignment(prob = 0.3) )
#' 
#' comparison <- compare_diagnoses(design_a, design_b, sims = 40)
#'  
#' @export
compare_diagnoses <- function(design1,
                              design2, 
                              sims = 500,
                              bootstrap_sims = 100,
                              merge_by_estimator = TRUE){
  
  

  
  if(bootstrap_sims <= 99){
    stop("diagnoses must have at least 100 bootstrap simulations")
  }

   diagnosis1 <- diagnose_design(design1, 
                                  sims = sims, 
                                  bootstrap_sims =  bootstrap_sims)

    diagnosis2 <- diagnose_design(design2,
                                  sims = sims, 
                                  bootstrap_sims = bootstrap_sims)

  compare_diagnoses_internal(diagnosis1, diagnosis2, sims, bootstrap_sims, merge_by_estimator)
  
}

#' Internal method to compare diagnoses
#'
#' @param diagnosis1 A diagnosis.
#' @param diagnosis2 A diagnosis.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}
#' @return A list with a data frame of compared diagnoses and both diagnoses.
#' 
#' 
#' @keywords internal
compare_diagnoses_internal <- function(diagnosis1, diagnosis2, sims,  bootstrap_sims, merge_by_estimator){
  
  diagnosands <- intersect(diagnosis1$diagnosand_names, diagnosis2$diagnosand_names)
  
  # Get merge_set ----
  # merge_by_set, used to merge diagnosands_df, must at least contain estimand_label
  # At its largest possible cardinality, merge_by_set contains c('estimand_label', 'estimator_label', 'term')
  group_by_set1 <- diagnosis1$group_by_set
  group_by_set2 <- diagnosis2$group_by_set
  estimand_in_set  <- "estimand_label" %in% group_by_set1 & "estimand_label" %in% group_by_set2
  estimator_in_set <- "estimator_label" %in% group_by_set1 & "estimator_label" %in% group_by_set2
  merge_by_set <- NULL
  
  # Stop if neither estimand_label nor estimator_label are included in both diagnoses
  if ((estimand_in_set + estimator_in_set) < 1){
    stop("Both diagnosands_df must contain at least estimand_label or estimator_label")
  }
  
  # Start adding names to the set of columns used for merging
  if (estimand_in_set){ 
    merge_by_set <- c("estimand_label")
  }
  
  if (merge_by_estimator) {
    if (!estimator_in_set ){
      warning("Estimator label not used for merging.
              At least one of the designs doesn't contain estimator_label.")
    } else {
      merge_by_set <- c(merge_by_set, "estimator_label")
    }
  }

  if ("term" %in% group_by_set1 & "term" %in% group_by_set2){ 
    merge_by_set <- c(merge_by_set, "term")
  }
  
  # Merge diagnoses
  # stop if no matching found
  # drop all columns that don't match
  comparison_df <- merge(diagnosis1$diagnosands_df, 
                         diagnosis2$diagnosands_df,
                         by = merge_by_set, 
                         suffixes = c("_1", "_2"),
                         stringsAsFactors = FALSE)

  if (nrow(comparison_df) == 0) {
    stop("Can't merge diagnosands data frames. Diagnoses don't have labels in common")
  }
  
  c_names <- colnames(comparison_df)
  filter_se <- grepl("^se", c_names)
  suffix <- c("_1", "_2") 
  keepcols <- grepl("[[:digit:]]+$", c_names)  |  grepl("label+$", c_names)| grepl("term", c_names) | filter_se
  comparison_df <- comparison_df[, keepcols]
  
  # Divide comparison_df into groups defined by set= c(estimand_label, estimator_label, term)
  c_names <- colnames(comparison_df)
  set <- grepl("^estimand_label", c_names)| grepl("^estimator_label", c_names)| grepl("^term", c_names) 
  comparison_list <- comparison_df[, set, drop = FALSE]
  comparison_list <- split(comparison_df, lapply( comparison_list, addNA), drop = TRUE)
  
  
  # Function to split bootstrap into groups defined by merge by set
  split_bootrstrap <- function(diagnosis, group_by_set = merge_by_set){
    bootstrap_df <- diagnosis$bootstrap_replicates
    group_by_list <- bootstrap_df[, group_by_set, drop = FALSE]
    split(bootstrap_df, lapply(group_by_list, addNA), drop = TRUE)
  }
  bootstrap_df1 <- split_bootrstrap(diagnosis1)
  bootstrap_df2 <- split_bootrstrap(diagnosis2)
  
  # Function to create list with output.
  # For each combination of estimand_label, estimator_label and term in comparison_list
  # take the mean and the se of each estimated diagnosand "d" from comparison_list
  # and unite with 
  # the mean difference computed as mean(bootstrap_df2[,"d"] - bootstrap_df1[, "d"]
  # se of the difference computed as sd(bootstrap_df2[,"d"] - bootstrap_df1[, "d"])
  # p.value computed as the proportion of times that the difference is on opposite side of zero from mean difference 
  create_difference_df <- function(label, 
                                   df1 = bootstrap_df1, 
                                   df2= bootstrap_df2,
                                   comparison = comparison_list,
                                   diagnosands_list = diagnosands, 
                                   group_by_set = merge_by_set) {
    # Estimated diagnosands and se(diagnosands) from each design
    d1 <- df1[[label]]
    d2 <- df2[[label]]
    comp <- comparison_list[[label]]
    c_names <- colnames(comp)
    set <-   grepl("^estimand_label", c_names)| grepl("^estimator_label", c_names)| grepl("^term", c_names) 
    select_cols <- c_names[grepl("^design_label", c_names)]
    select_cols <- c(select_cols, c_names[set])
    identifiers <- comp[,   select_cols]
    
    # Compute difference stats
    difference <- do.call(rbind, lapply(diagnosands_list, function(diagnosand){
      diagnosand1 <- comp[,paste0(diagnosand, "_1")]
      diagnosand2 <- comp[,paste0(diagnosand, "_2")]
      se1 <- comp[,paste0("se(",diagnosand, ")_1")]
      se2 <- comp[,paste0("se(",diagnosand, ")_2")]
      diff <- d2[, diagnosand] - d1[, diagnosand] 
      mu <- mean(diff) 
      p.value <- ifelse(sign(mu) == -1, mean(diff > 0),  mean(diff < 0))
      
      # Combine
     out<- data.frame(diagnosand =diagnosand,
                 mean_1 = diagnosand1,
                 mean_2 = diagnosand2,
                 mean_difference = mu ,
                 se_1 = se1,
                 se_2 = se2,
                 se_difference = sd(diff),
                 p.value = p.value,
                 sims = sims, 
                 bootstrap_sims = bootstrap_sims,
                stringsAsFactors = FALSE)
     
      }))
    suppressWarnings(cbind(identifiers, difference))

  }
  
  return_df <- lapply(names(comparison_list),  create_difference_df )
  return_df <- data.frame(rbind_disjoint(return_df, infill = ""))

  
  return_list <- list(compared_diagnoses_df =  return_df, 
                      diagnosis1 = diagnosis1, 
                      diagnosis2 = diagnosis2)
  
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
print.summary.compared_diagnoses <- function(x, ...){
  
  bootstrap_sims <- x$diagnosis1$bootstrap_sims
  sims <- nrow(x$diagnosis1$simulations_df)
  compared_diagnoses_df <- x[["compared_diagnoses_df"]]

  column_names <- colnames(compared_diagnoses_df)
  identifiers <- column_names %in% c("estimand_label", "term") | grepl("^estimator_label", column_names)
  identifiers_columns <- column_names[identifiers]
  p.value <- compared_diagnoses_df$p.value
  stars <- ifelse(p.value < 0.01 ,"***",
                  ifelse(p.value < 0.05 & p.value>=0.01, "**",
                         ifelse(p.value < 0.1 & p.value>=0.05, "*", "")))
 
  # Make diagnosand only df
  clean_comparison_df <- compared_diagnoses_df[, c(identifiers_columns, "diagnosand", "mean_1", "mean_2", "mean_difference"), drop = FALSE]
  
  clean_values_df <- data.frame(lapply(clean_comparison_df[, c("mean_1", "mean_2", "mean_difference"), drop = FALSE], format_num, 
                                       digits = 2), stringsAsFactors = FALSE)
  clean_values_df$mean_difference <- paste0(clean_values_df$mean_difference, stars)
  diagnosands_only_df <- cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], clean_values_df)
  
  colnames(diagnosands_only_df)[colnames(diagnosands_only_df) %in% c("mean_1", "mean_2", "mean_difference")] <- c("design1", "design2", "difference")
  
  
  # Make se only df
  se_only_df <- compared_diagnoses_df[, grepl("^se", colnames(compared_diagnoses_df)), drop = FALSE]

  se_only_df <- data.frame(lapply(se_only_df, function(se){ 
                                   paste0("(", format_num(se, digits = 2), ")")}),
                           stringsAsFactors = FALSE)
  
  se_only_df <- cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], se_only_df)
 
  colnames(se_only_df) <- colnames(diagnosands_only_df)
  
  
  # Combine  mean and ses of diagnosands and difference
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")
  return_df <- return_df[do.call(order, as.list(return_df[, c(identifiers_columns, "diagnosand")])), , drop = FALSE]
  r <- nrow(return_df)
  return_df[(1:r)%%2 == 0, c(identifiers_columns, "diagnosand")] <- ""
  
  # Print and return
  cat(paste0("\n Comparison of research designs diagnoses based on ", sims, " simulations"))
  
  cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (", bootstrap_sims, 
             ")."))
 
  cat("\n\n")
  print(return_df, row.names = FALSE)
 cat("\nNote: *p < 0.10, ** p < 0.05, *** p < 0.01")
  invisible(return_df)
}


