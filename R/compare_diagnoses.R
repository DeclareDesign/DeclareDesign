#' Compare Diagnoses
#'
#' Diagnose and compare designs.
#'
#' @param base_design A design or a diagnosis.
#' @param comparison_design A design or a diagnosis.
#' @param sims The number of simulations, defaulting to 1000. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}. Used for both designs.
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{1000}. Set to FALSE to turn off bootstrapping. Used for both designs.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{TRUE}
#' @param alpha The confidence level, 0.01 by default.
#'  @return A list with a data frame of compared diagnoses and both diagnoses.
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
                              bootstrap_sims = 1000, 
                              alpha = 0.01,
                              merge_by_estimator = TRUE){
  
  
  if(bootstrap_sims <= 99) stop("Please choose a higher number of bootstrap simulations")
  
  
 # Diagnose designs if base/comparison are of class "design"
  
  if( "design" %in% class(base_design)  ){
   
    diagnosis1 <- diagnose_design(base_design, 
                                  sims = sims, 
                                  bootstrap_sims = bootstrap_sims)

    } else if(class(base_design) == "diagnosis") {
      if(base_design$bootstrap_sims<= 99)
        stop("base_design must have a higher number of bootstrap simulations")
    diagnosis1 <- base_design} else{ 
    stop("base_design must be either a design or a diagnosis")}
  

  if( "design" %in% class(comparison_design)  ){
    
    diagnosis2 <- diagnose_design(comparison_design,
                                  sims = sims, 
                                  bootstrap_sims = 100)
    
    } else if(class(comparison_design) == "diagnosis"){
      diagnosis2 <- comparison_design
      } else{ 
        stop("comparison_design must be either a design or a diagnosis")}
  
  
  out <- compare_diagnoses_internal(diagnosis1, diagnosis2, merge_by_estimator, alpha )
  
  out
}





#' @export
compare_diagnoses_internal <- function(diagnosis1, diagnosis2, merge_by_estimator, alpha ) {
  
  # 1.Housekeeping
  if(is.null(diagnosis1$bootstrap_replicates ) )
    stop("Can't compare diagnoses witouth bootstrap replicates")
  
  
  if(length(diagnosis1$parameters_d[, "design_label"])  + length(diagnosis2$parameters_d[, "design_label"])> 2) 
    stop("Please only send design or diagnosis objects with one unique design_label.")

  diagnosands  <- base::intersect(diagnosis1$diagnosand_names, 
                                  diagnosis2$diagnosand_names)
  
  
  # merge_by_set, used to merge diagnosands_df, must at least contain estimand_label
  # at its largest possible cardinality, merge_by_set contains c("estimand_label", "term", " "estimator_label"") 
  # if group_by_set is different in both design, merge_by_set contains labels present in both
  # NOTE: Need to test how comparison is done when only on diagnosis has term or estimator_label
  
  group_by_set1 <- diagnosis1$group_by_set
  group_by_set2 <- diagnosis2$group_by_set
  
  mand  <-  "estimand_label"  %in% group_by_set1 & "estimand_label"  %in% group_by_set2
  mator <-  "estimator_label" %in% group_by_set1 & "estimator_label" %in% group_by_set2
  if( mand + mator < 1) 
    stop(paste0("diagnosands_df must contain at least estimand_label or estimator_label in common" ))
  
  merge_by_set <- NULL

  if(mand) merge_by_set <- c("estimand_label")
  if(merge_by_estimator){
    if(!mator)
      warning("Estimator label not used in columns for merging.
              At least one of the designs doesn't contain estimator_label.")
    else
   merge_by_set <- c(merge_by_set, "estimator_label")
  }
  
  if("term" %in% group_by_set1 & "term" %in% group_by_set2) 
    merge_by_set <- c(merge_by_set, "term")

   
  comparison_df <-  merge(diagnosis1$diagnosands_df  , diagnosis2$diagnosands_df, 
                          by = merge_by_set, suffixes = c("_1", "_2"), stringsAsFactors = FALSE)
  if(nrow(comparison_df) == 0 ){
    warning("Can't merge diagnosands data frames. Diagnoses don't have labels in common")
   return(NULL)
    
  }
    
  c_names <- colnames(comparison_df)
  suffix <- c("_1", "_2")
  
  dropcols <- grepl("[[:digit:]]+$", c_names ) |   c_names  %in% c("design_label", "estimand_label", "estimator_label", "term") 
  
  
  # Grab standard.error columns before droping cols that are not present in both diagnosands_df
  # just in case diagnosis2 doesn't  include bootrstrap_replicates
  filter_se      <- grepl("^se",  c_names)
  diagnosands_se <-  comparison_df[, filter_se]
  if(diagnosis2$bootstrap_sims == 0){
  diagnosands_se1 <- diagnosands_se; diagnosands_se2 <- matrix(data = rep(NA, length(diagnosands_se)))
  } else { 
    diagnosands_se1 <-  diagnosands_se[,  grepl("_1$",  colnames(diagnosands_se))]
    diagnosands_se2 <-  diagnosands_se[,  grepl("_2$",  colnames(diagnosands_se))]}
  
  
  comparison_df  <- comparison_df[, dropcols]
  
  # Compute bootstrap confidence interval
 
  bootstrap_df  <- diagnosis1$bootstrap_replicates
  group_by_list <- bootstrap_df[,  group_by_set1, drop = FALSE]
  labels_df     <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df     <- lapply(labels_df, head, n = 1)
  bootstrap_df  <- split(bootstrap_df , lapply(group_by_list, addNA), drop = TRUE)
  group_by_set  <- diagnosis1$group_by_set
  lower.bound   <- lapply(bootstrap_df,FUN = function(x) {
    d <- x[, diagnosands]
    set <- head(x[,group_by_set], 1)
    q <- sapply(d, function(d) quantile(d, c(lower = 0.5*alpha), na.rm = TRUE))
    q <- setNames(q, diagnosands)
    q <- cbind(set, t(q))
  })

  upper.bound <- lapply(bootstrap_df,FUN = function(x) {
     d <- x[, diagnosands]
     set <- head(x[,group_by_set], 1)
     q <- sapply(d, function(d) quantile(d, 1-0.5*alpha, na.rm = TRUE))
     q <- setNames(q, diagnosands)
     q <- cbind(set, t(q))
   })
  
  
  lower.bound <- rbind_disjoint(lower.bound)
  upper.bound <- rbind_disjoint(upper.bound)
  
  
  # Split columns by diagnosands
  filter_mean      <- gsub("_[[:digit:]]+$","", colnames(comparison_df)) %in%  diagnosands
  diagnosands_mean <- comparison_df[,  filter_mean]
  filter_sims      <- grepl("^n_sims", colnames(comparison_df))
  sims_df          <- comparison_df[,filter_sims]
  filter_se        <- grepl("^se",  colnames(comparison_df))
  comparison_df    <- comparison_df[!filter_mean &   !filter_se & !filter_sims]
  
  # Suppress warnings on rownames 
  out <- suppressWarnings( do.call(rbind, lapply(1:nrow(diagnosands_mean), function(pair){
   data.frame(comparison_df[pair, ], 
              diagnosand = diagnosands,
              t(mapply(function(mean_1, mean_2, se_1, se_2, l, u, n1 , n2) 
                c(mean_1 = mean_1, mean_2 = mean_2, se_1 = se_1, se_2 = se_2,
                  n_sims1 = n1, n_sims2 = n2, conf.low_1 = l, conf.upper_1 =  u, 
                  in_interval =  ((mean_2 >= l) & (mean_2 <= u))), 
                  #args
                  mean_1 = diagnosands_mean[pair, 1:length(diagnosands)],
                  mean_2 = diagnosands_mean[pair, (length(diagnosands)+1):ncol(diagnosands_mean)],
                  se_1 = diagnosands_se1[pair,],
                  se_2 =  diagnosands_se2[pair,],
                  l =  lower.bound[pair, diagnosands],
                  u =  upper.bound[pair, diagnosands],
                  n1 = sims_df[pair, 1],
                  n2 = sims_df[pair, 2])),
                
               stringsAsFactors = FALSE)})))
  
  c_names <- colnames(out)
  
  #comparison_df <-  rapply(object = out , f = round, classes = "numeric", how = "replace", digits = 2) 
  comparison_df <- out
  
  # Prepare output
  # Add "_1" or "_2" to labels as needed.
  if("estimand_label" %in% c_names & ! "estimand_label" %in% merge_by_set)
    c_names[c_names == "estimand_label"] <- paste0("estimand_label", suffix[c( "estimand_label" %in% group_by_set1, "estimand_label" %in% group_by_set2)])
  if("estimator_label" %in% c_names & ! "estimator_label" %in% merge_by_set)
    c_names[c_names == "estimator_label"] <- paste0("estimator_label", suffix[c( "estimator_label" %in% group_by_set1, "estimator_label" %in% group_by_set2)])
  if("term" %in% c_names & !"term" %in% merge_by_set)
    c_names[c_names == "term"] <- paste0("term", suffix[c("term" %in% group_by_set1, "term" %in% group_by_set2)])
  comparison_df <- setNames( comparison_df,  c_names)
  
  if( all(is.na(comparison_df$se_2))) comparison_df$se_2 <- NULL
  out <- list(compared.diagnoses_df =comparison_df ,
              diagnosis1 = diagnosis1,
              diagnosis2 = diagnosis2)
  
  class(out) <- "compared.diagnoses"
  
  invisible(out)
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
  
  x  <- x[["compared.diagnoses_df"]]
  sx <- subset(x,  x[,"in_interval"] == 0)
  if(nrow(sx) == 0) {
    print("No divergences found.") 
    #print(x)
    return(x)}
  if(n_sims1 == n_sims2 )
    cat(paste0("\n Comparison of research designs diagnoses based on ", n_sims1, " simulations."))
  else 
    cat(paste0("\n Comparison of research designs diagnoses based on ", n_sims1, " simulations from `design_1`` and ", n_sims2, " from `design_2`."))
  

  

  if(bootstrap_rep1 == bootstrap_rep2)
    cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (", bootstrap_rep1,")."  ))
  else 
    cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (design_1 = ", bootstrap_rep1,", design_1 = ", bootstrap_rep2, ")."  ))
 
  se_1 <- paste0("(",format_num(sx$se_1, 2), ")")
  se_2 <- paste0("(",format_num(sx$se_2, 2), ")")
  sx   <- sx[ ,!colnames(sx) %in% c("in_interval", "se_1", "se_2")]

  mand  <- startsWith(colnames(sx), "estimand")
  term  <- startsWith(colnames(sx), "term")
  mator <- startsWith(colnames(sx), "estimator")
  n_labels <- sum(mand, mator, term)
  r <- nrow(sx)
  k <- ncol(sx)
  diagnosand_se <- paste0("se(", sx$diagnosand, ")")
  sx <- sx[, 1:(k-4)]
  k <- ncol(sx)
  label_cols <- matrix(replicate(n_labels + 1 , rep("", r)), ncol =  n_labels + 1 )
  designs <- startsWith(colnames(sx), "design")
  
  se_rows <- data.frame(
        label_cols,
        se_1,
        se_2, stringsAsFactors = FALSE)  
  
  out <- data.frame(rbind(sx, sx), stringsAsFactors = FALSE)
  diagnosands_cols <- startsWith(colnames(out), "mean")
  colnames(out)[diagnosands_cols] <- c("base", "comparison")
  m <- nrow(out)
  odd  <- (1:m) %% 2 != 0
  even <- (1:m) %% 2 == 0
  diagnosands_m <- sapply(sx[,diagnosands_cols], format_num, digits=2)
  
  out[odd, diagnosands_cols ]  <- diagnosands_m
  out[even, !designs] <- se_rows
  out[odd, "diagnosand"] <- sx$diagnosand
  out[,  designs] <- 
    sapply(out[, designs] , function(x) factor(x, levels = c(levels(x), " ")))
  out[even,  designs] <-  " "
    
    
  cat("\n\n")
  print(out, row.names = FALSE)
  cat(paste0("\n\n Displaying diagnosands that statistically diverge between base and comparison designs. See help file for details." ))
  invisible(out)
}



