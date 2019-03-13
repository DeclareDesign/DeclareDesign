#' Compare Diagnoses
#'
#' Diagnose and compare designs.
#'
#' @param base_design A design or a diagnosis.
#' @param comparison_design A design or a diagnosis.
#' @param sims The number of simulations, defaulting to 1000. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}. Used for both designs.
#' @param bootstrap_sims_base Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{1000}. Set to FALSE to turn off bootstrapping. Used for base_design.
#' @param bootstrap_sims_comparison Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{1000}. Set to FALSE to turn off bootstrapping. Used for comparison_designs.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}
#' @param alpha The confidence level, 0.01 by default.
#' @return A list with a data frame of compared diagnoses and both diagnoses.
#'
#' @details
#' 
#' The function \code{compare_diagnoses()} runs a many-to-many merge matching by \code{estimand_label} and \code{term} (if present). If  \code{merge_by_estimator} equals \code{TRUE}, \code{estimator_label} is also included in the merging condition. Any diagnosand that is not included in both designs will be dropped from the merge.
#' 
#' The data frame of compared diagnoses has a column, \code{in_interval}, that indicates if two given diagnosands diverge statistically. 
#' i.e. if a diagnosand from \code{comparison_design} is not contained in the 95\% bootstrap confidence interval of their equivalent diagnosand from \code{base_design}. The column \code{in_interval} equals zero for divergent diangnosands and one otherwise..
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
#' compare_diagnoses(design_a, design_b)
#'  
#' @export
compare_diagnoses <- function(base_design,
                              comparison_design, 
                              sims = 1000,
                              bootstrap_sims_base = 1000, 
                              bootstrap_sims_comparison = 100,
                              alpha = 0.01,
                              merge_by_estimator = TRUE){
  
  
  if(bootstrap_sims_base <= 99){
    stop("base_design must have at least 100 bootstrap simulations")
  }
  
 # Diagnose designs if base/comparison are of class "design"
 # Or save and later pass object if class is "diagnosis" 
 # or stop if none of the above
  
  # base_design 
  if(inherits(base_design, "design")){
    diagnosis1 <- diagnose_design(base_design, 
                                  sims = sims, 
                                  bootstrap_sims =  bootstrap_sims_base)
  } else {
    if(class(base_design) == "diagnosis"){
      if(base_design$bootstrap_sims<= 99) 
       stop("base_design must have at least 100 bootstrap simulations")
      diagnosis1 <- base_design
    } else {
      stop("base_design must be either a design or a diagnosis")
    }
  }
  
    # comparisons_design 
  if(inherits(comparison_design, "design")){
    diagnosis2 <- diagnose_design(comparison_design,
                                  sims = sims, 
                                  bootstrap_sims = bootstrap_sims_comparison)
  } else { 
    if(class(comparison_design) == "diagnosis"){
      diagnosis2 <- comparison_design
    } else{
    stop("comparison_design must be either a design or a diagnosis")
    }
  }  
  
  compare_diagnoses_internal(diagnosis1, diagnosis2, merge_by_estimator, alpha)
  
}


#' Internal method to compare diagnoses
#'
#' @param diagnosis1 A diagnosis.
#' @param diagnosis2 A diagnosis.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}
#' @param alpha The confidence level, 0.01 by default.
#' @return A list with a data frame of compared diagnoses and both diagnoses.
#' @keywords internal

compare_diagnoses_internal <- function(diagnosis1, diagnosis2, merge_by_estimator, alpha) {
  
  np1 <- length(diagnosis1$parameters_df[, "design_label"])
  np2 <- length(diagnosis2$parameters_df[, "design_label"])
  
  if(np1 + np2  > 2){
    stop("Please only send design or diagnosis objects with one unique design_label.")
  }
  diagnosands <- intersect(diagnosis1$diagnosand_names, diagnosis2$diagnosand_names)
  
  # merge_by_set, used to merge diagnosands_df, must at least contain estimand_label at its
  # largest possible cardinality, merge_by_set contains c('estimand_label', 'term', '
  # 'estimator_label') if group_by_set is different in both designs, merge_by_set contains
  # labels the intersection from both.  NOTE: Need to test how comparison is done when only
  # on diagnosis has term or estimator_label
  
  group_by_set1 <- diagnosis1$group_by_set
  group_by_set2 <- diagnosis2$group_by_set
  
  estimand_in_set  <- "estimand_label"  %in% group_by_set1 & "estimand_label" %in% group_by_set2
  estimator_in_set <- "estimator_label" %in% group_by_set1 & "estimator_label" %in% group_by_set2
  
  merge_by_set <- NULL
  
  if (estimand_in_set + estimator_in_set < 1){
    stop("diagnosands_df must contain at least estimand_label or estimator_label in common")
  }
  
  if (estimand_in_set){ 
    merge_by_set <- c("estimand_label")
  }
  
  if (merge_by_estimator) {
    if (!estimator_in_set){
      warning("Estimator label not used for merging.
              At least one of the designs doesn't contain estimator_label.")
    } else {
      merge_by_set <- c(merge_by_set, "estimator_label")
    }
  }
  
  
  if ("term" %in% group_by_set1 & "term" %in% group_by_set2){ 
    merge_by_set <- c(merge_by_set, "term")
  }
  
  comparison_df <- merge(diagnosis1$diagnosands_df, 
                         diagnosis2$diagnosands_df,
                         by = merge_by_set, 
                         suffixes = c("_1", "_2"),
                         stringsAsFactors = FALSE)
  
  if (nrow(comparison_df) == 0) {
    warning("Can't merge diagnosands data frames. Diagnoses don't have labels in common")
    return(list(diagnosis1 = diagnosis1, diagnosis2 = diagnosis2))
  }
  
  c_names <- colnames(comparison_df)
  filter_se <- grepl("^se", c_names)
  suffix <- c("_1", "_2") 
  keepcols <- grepl("[[:digit:]]+$", c_names)  |  grepl("label+$", c_names)| grepl("term", c_names) | filter_se
  comparison_df <- comparison_df[, keepcols]
  c_names <- colnames(comparison_df)             
  diagnosands_se <- comparison_df[, filter_se]
  
  if (diagnosis2$bootstrap_sims == 0) {
    diagnosands_se1 <- diagnosands_se
    diagnosands_se2 <- matrix(data = rep(NA, length(diagnosands_se)))
  } else {
    diagnosands_se1 <- diagnosands_se[, endsWith(colnames(diagnosands_se), "_1")]
    diagnosands_se2 <- diagnosands_se[, endsWith(colnames(diagnosands_se), "_2")]
  }
  
  
  # Compute bootstrap confidence interval
  bootstrap_df <- diagnosis1$bootstrap_replicates
  group_by_list <- bootstrap_df[, group_by_set1, drop = FALSE]
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  bootstrap_df <- split(bootstrap_df, lapply(group_by_list, addNA), drop = TRUE)
  group_by_set <- diagnosis1$group_by_set
  
  compute_bound <- function(x, 
                            bound,
                            diagnosands_list = diagnosands,
                            group = group_by_set) {
    d <- x[, diagnosands_list]
    set <- head(x[, group], 1)
    q <- sapply(d, function(d) quantile(d, bound, na.rm = TRUE))
    q <- setNames(q, diagnosands_list)
    q <- cbind(set, t(q))
  }
  
  lower.bound <- lapply(bootstrap_df, compute_bound, bound =  0.5 * alpha )
  upper.bound <- lapply(bootstrap_df, compute_bound, bound = 1 - 0.5 * alpha)
  lower.bound <- rbind_disjoint(lower.bound)
  upper.bound <- rbind_disjoint(upper.bound)
  
  # Split columns by diagnosands
  filter_mean <- gsub("_[[:digit:]]+$", "", colnames(comparison_df)) %in% diagnosands
  diagnosands_mean <- comparison_df[, filter_mean]
  diagnosands_mean1 <- diagnosands_mean[, endsWith(colnames(diagnosands_mean), "_1")]
  diagnosands_mean2 <- diagnosands_mean[, endsWith(colnames(diagnosands_mean), "_2")]
  filter_sims <- grepl("^n_sims", colnames(comparison_df))
  sims_df <- comparison_df[, filter_sims]
  filter_se <- grepl("^se", colnames(comparison_df))
  identifiers_df <- comparison_df[!filter_mean & !filter_se & !filter_sims]
  n_diagnosands <- length(diagnosands)
  
  # Reshape data from wide to long 
  # create a row for each diagnosand of a given estimand/estimatorcomparison
  # can optionally use gather(); it'd make code longer but perhaps easier to read
  # suggestions welcome
  # Suppress warnings on rownames
  
  comparison_df <-
  suppressWarnings(do.call(rbind, lapply(1:nrow(diagnosands_mean),function(pair){
    
    data.frame(identifiers_df[pair, ], diagnosand = diagnosands,
               t(mapply(
                 
                 function(mean_1, mean_2, se_1, se_2, l, u, n1, n2){
                   c(mean_1 = mean_1, 
                     mean_2 = mean_2,
                     se_1 = se_1,
                     se_2 = se_2, 
                     n_sims1 = n1, 
                     n_sims2 = n2, 
                     conf.low_1 = l,
                     conf.upper_1 = u, 
                     in_interval = ((mean_2 >= l) & (mean_2 <= u)))
                 },
                 
                 # Mapply arguments
                 mean_1 = diagnosands_mean1, 
                 mean_2 = diagnosands_mean2, 
                 se_1 = diagnosands_se1[pair, ],
                 se_2 = diagnosands_se2[pair,], 
                 l = lower.bound[pair, diagnosands], 
                 u = upper.bound[pair, diagnosands],
                 n1 = sims_df[pair, 1],
                 n2 = sims_df[pair, 2])), 
               stringsAsFactors = FALSE)
  })))
  
  c_names <- colnames(comparison_df)
  
  # Prepare output
  # merge adds automatically suffix if column names are repeated in both diagnosands_df. 
  # If not add "_1" or "_2" manually as needed.
  if("estimand_label" %in% c_names & ! "estimand_label" %in% merge_by_set){
    c_names[c_names == "estimand_label"] <- paste0("estimand_label", suffix[c( "estimand_label" %in% group_by_set1, "estimand_label" %in% group_by_set2)])
  }
  
  if("estimator_label" %in% c_names & ! "estimator_label" %in% merge_by_set){
    c_names[c_names == "estimator_label"] <- paste0("estimator_label", suffix[c( "estimator_label" %in% group_by_set1, "estimator_label" %in% group_by_set2)])
  }
  
  if("term" %in% c_names & !"term" %in% merge_by_set){
    c_names[c_names == "term"] <- paste0("term", suffix[c("term" %in% group_by_set1, "term" %in% group_by_set2)])
  }
  
  comparison_df <- setNames(comparison_df,  c_names)
  
  # if user passes diagnosis2 without bootstrap sims se_2 delete column
  if (all(is.na(comparison_df$se_2))) comparison_df$se_2 <- NULL

  return_list <- list(compared.diagnoses_df = comparison_df, 
                      diagnosis1 = diagnosis1, 
                      diagnosis2 = diagnosis2)
  
  attr(return_list, "alpha") <- alpha
  
  class(return_list) <- "compared.diagnoses"
  
  return_list
  }


#' @export
print.compared.diagnoses <- function(object, ...) {
  print(summary(object))
  invisible(object)
}


#' @export
summary.compared.diagnoses <- function(x, ...) {
  structure(x, class = c("summary.compared_diagnoses", "data.frame"))
  
}



#' @export
print.summary.compared_diagnoses <- function(x, ...){
  
  bootstrap_rep1 <- x$diagnosis1$bootstrap_sims
  bootstrap_rep2 <- x$diagnosis2$bootstrap_sims
  n_sims1 <- nrow(x$diagnosis1$simulations_df)
  n_sims2 <- nrow(x$diagnosis2$simulations_df)
  compared.diagnoses_df <- x[["compared.diagnoses_df"]]
  compared.diagnoses_df <- subset(compared.diagnoses_df, compared.diagnoses_df[, "in_interval"] == 0)
  alpha <- attr(x, "alpha")
  atext <- paste0("(confidence level = ", alpha, ")")
  
  if (nrow(compared.diagnoses_df) == 0) {
    print(paste("No visible differences between objects (at a confidence level =", atext,")"))
    return(x)
  }
  
  
  if (n_sims1 == n_sims2) {
    cat(paste0("\n Comparison of research designs diagnoses based on ", n_sims1, " simulations", atext))
  } else {
    cat(paste0("\n Comparison of research designs diagnoses based on ", n_sims1, " simulations from `base_design` and ", 
               n_sims2, " from `comparison_design`", atext))
  }
  if (bootstrap_rep1 == bootstrap_rep2) {
    cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (", bootstrap_rep1, 
               ")."))
  } else {
    cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (base_design = ", bootstrap_rep1, 
               ", comparison_design = ", bootstrap_rep2, ")."))
  }
  
  
  column_names <- colnames(compared.diagnoses_df)
  identifiers <- column_names %in% c("estimand_label", "term") | grepl("^estimator_label", column_names)
  identifiers_columns <- column_names[identifiers]
  
  # Make diagnosand only df
  clean_comparison_df <- compared.diagnoses_df[, c(identifiers_columns, "diagnosand", "mean_1", "mean_2"), drop = FALSE]
  
  
  clean_values_df <- data.frame(lapply(clean_comparison_df[, c("mean_1", "mean_2"), drop = FALSE], format_num, 
                                       digits = 2), stringsAsFactors = FALSE)
  
  diagnosands_only_df <- cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], clean_values_df)
  
  colnames(diagnosands_only_df)[colnames(diagnosands_only_df) %in% c("mean_1", "mean_2")] <- c("base", "comparison")
  # Make se only df
  se_only_df <- compared.diagnoses_df[, grepl("^se", colnames(compared.diagnoses_df)), drop = FALSE]
  
  
  se_only_df <- data.frame(lapply(se_only_df, function(se) {
    paste0("(", format_num(se, digits = 2), ")")
    
  }), stringsAsFactors = FALSE)
  
  # Add se_2 colum
  se_only_df <- cbind(clean_comparison_df[, c(identifiers_columns, "diagnosand"), drop = FALSE], se_only_df)
  if(!"se_2" %in% colnames(se_only_df)){
    se_only_df$se_2 <- "-"
  }
  
  colnames(se_only_df) <- colnames(diagnosands_only_df)
  
  # Merge
  return_df <- rbind_disjoint(list(diagnosands_only_df, se_only_df), infill = "")
  
  return_df <- return_df[do.call(order, as.list(return_df[, c(identifiers_columns, "diagnosand")])), , drop = FALSE]
  r <- nrow(return_df)
  return_df[(1:r)%%2 == 0, c(identifiers_columns, "diagnosand")] <- ""
  
  
  cat("\n\n")
  print(return_df, row.names = FALSE)
  cat(paste0("\n\n Displaying diagnosands of the comparison design that lie outside the ", (1-alpha)*100 ,"% bootstrap confidence interval of their counterparts in the base_design."))
  invisible(return_df)
}



