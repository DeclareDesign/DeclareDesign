#' Compare Diagnoses
#'
#' Diagnose and compare designs.
#'
#' @param design_or_diagnosis1 A design or a diagnosand.
#' @param design_or_diagnosis2 A design or a diagnosand.
#' @param sims The number of simulations, defaulting to 500. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}. Used for both designs.
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{100}. Set to FALSE to turn off bootstrapping. Used only for \code{design_or_diagnosis1}.
#' @param merge_by_estimator A logical. Whether to include \code{estimator_label} in the set of columns used for merging. Defaults to \code{FALSE}
#' @return A list with a data frame of simulations, a data frame of diagnosands, a vector of diagnosand names, and if calculated, a data frame of bootstrap replicates.
#'
#'
#' @export
compare_diagnoses <- function(design_or_diagnosis1,
                              design_or_diagnosis2, 
                              sims = 500,
                              bootstrap_sims = 100, 
                              merge_by_estimator = FALSE){
  
  
  if(bootstrap_sims <= 0) stop("Please choose a higher number of bootstrap simulations")
  
  
 # Diagnose designs design_or_diagnoses are design object
  if( "design" %in% class(design_or_diagnosis1)  ){
   
     
    design_1 <- design_or_diagnosis1
    diagnosis1 = diagnose_design(design_1, 
                                 sims = sims, 
                                 bootstrap_sims = bootstrap_sims)

    } else if(class(design_or_diagnosis1) == "diagnosis") {
      if(design_or_diagnosis1$bootstrap_sims<= 0) stop("design_or_diagnosis1 must have a higher number of bootstrap simulations")
    diagnosis1 <- design_or_diagnosis1} else{ 
    stop("design_or_diagnosis1 must be either a design or a diagnosis")}
  
  
  
  if( "design" %in% class(design_or_diagnosis2)  ){
    design_2 <- design_or_diagnosis2
    diagnosis2 = diagnose_design(design_2,
                                 sims = sims, bootstrap_sims = bootstrap_sims)
    } else if(class(design_or_diagnosis2) == "diagnosis"){
      diagnosis2 <- design_or_diagnosis2
      } else{ 
        stop("design_or_diagnosis2 must be either a design or a diagnosis")}
  
  
  out <- compare_diagnoses_internal(diagnosis1, diagnosis2, merge_by_estimator )
  class(out) <- "compared.diagnoses"
  out
}





#' @export
compare_diagnoses_internal <- function(diagnosis1, diagnosis2, merge_by_estimator ) {
  
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
  
  # Compute bootstrap confindence interval
  bootstrap_df  <- diagnosis1$bootstrap_replicates
  group_by_list <- bootstrap_df[,  group_by_set1, drop = FALSE]
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  bootstrap_df <- split(bootstrap_df , lapply(group_by_list, addNA), drop = TRUE)
  group_by_set <- diagnosis1$group_by_set
  lower.bound <- lapply(bootstrap_df,FUN = function(x) {
    d <- x[, diagnosands]
    set <- head(x[,group_by_set], 1)
    q <- sapply(d, function(d) quantile(d, c(lower = 0.05), na.rm = TRUE))
    q <- setNames(q, diagnosands)
    q <- cbind(set, t(q))
  })

  upper.bound <- lapply(bootstrap_df,FUN = function(x) {
     d <- x[, diagnosands]
     set <- head(x[,group_by_set], 1)
     q <- sapply(d, function(d) quantile(d, 0.99, na.rm = TRUE))
     q <- setNames(q, diagnosands)
     q <- cbind(set, t(q))
   })
  
  
  lower.bound <- rbind_disjoint(lower.bound)
  upper.bound <- rbind_disjoint(upper.bound)
  
  
  # split columns by diagnosands
  filter_mean      <- gsub("_[[:digit:]]+$","", colnames(comparison_df)) %in%  diagnosands
  diagnosands_mean <- comparison_df[,  filter_mean]
  filter_sims      <- grepl("^n_sims", colnames(comparison_df))
  sims_df          <- comparison_df[,filter_sims]
  filter_se        <- grepl("^se",  colnames(comparison_df))
  comparison_df    <- comparison_df[!filter_mean &   !filter_se & !filter_sims]
  
  # Suppres warnings on rownames 
  out <- suppressWarnings( do.call(rbind, lapply(1:nrow(diagnosands_mean), function(pair){
   data.frame(comparison_df[pair, ], 
              diagnosand = diagnosands,
              t(mapply(function(mean_1, mean_2, se_1, se_2, l, u) 
                c(mean_1 = mean_1, mean_2 = mean_2, se_1 = se_1, se_2 = se_2, 
                  in_interval =  mean_2 >= l & mean_2 <= u, conf.low_1 = l, conf.upper_1 = u), 
                  #args
                  mean_1 = diagnosands_mean[pair, 1:length(diagnosands)],
                  mean_2 = diagnosands_mean[pair, (length(diagnosands)+1):ncol(diagnosands_mean)],
                  se_1 = diagnosands_se1[pair,],
                  se_2 =  diagnosands_se2[pair,],
                  l =  lower.bound[pair, diagnosands],
                  u =  upper.bound[pair, diagnosands])),
                 sims_df[pair, ],
               stringsAsFactors = FALSE)})))
  
  c_names <- colnames(out)
  
  comparison_df <-  rapply(object = out , f = round, classes = "numeric", how = "replace", digits = 2) 
  
  
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
    invisible(list(compared.diagnoses_df =comparison_df ,
            diagnosis1 = diagnosis1,
            diagnosis2 = diagnosis2))
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
  n_sims1 <- nrow(comparison$diagnosis1$simulations_df)
  n_sims2 <- nrow(comparison$diagnosis2$simulations_df)
  if(n_sims1 == n_sims2 )
    cat(paste0("\n Comparison of research design diagnosis based on ", n_sims1, " simulations."))
  else 
    cat(paste0("\n Comparison of research design diagnosis based on ", n_sims1, " simulations from `design_1`` and ", n_sims2, " from `design_2`."))
 
  if(bootstrap_rep1  ==bootstrap_rep2)
    cat(paste0("\nDiagnosand estimates with bootstrapped standard errors in parentheses (", bootstrap_rep1,")."  ))
  else 
    cat(paste0("\n  Diagnosand estimates with bootstrapped standard errors in parentheses (design_1 = ", bootstrap_rep1,", design_1 = ", bootstrap_rep2, ")."  ))
 
  cat("\n\n" , sep = "")
  x  <- x[["compared.diagnoses_df"]]
  sx <- subset(x,  x[,"in_interval"] == 0)
  cols <- base::startsWith(colnames(x), "design_label")
  design_labels <-  sx[1, cols]
  sx <- sx[, !cols]
  se_1 <- paste0("(",sx$se_1, ")")
  se_2 <- paste0("(",sx$se_2, ")")
  sx$in_interval <- NULL
  sx$se_1 <- NULL
  sx$se_2 <- NULL
  mand  <- startsWith(colnames(x), "estimand")
  term  <- startsWith(colnames(x), "term")
  mator <- startsWith(colnames(x), "estimator")
  n_labels <- sum(mand, mator, term)
  r <- nrow(sx)
  k <- ncol(sx)
  diagnosand_se <- paste0("se(", sx$diagnosand, ")")
  sx <- sx[, 1:(k-4)]
  k <- ncol(sx)
  
  se_rows <- data.frame(
        replicate(n_labels + 1, rep("", r)),
        se_1,
        se_2, stringsAsFactors = FALSE)  
  
  
  out <- data.frame(rbind(sx, sx), stringsAsFactors = FALSE)
  colnames(out)[startsWith(colnames(out), "mean")] <- c("design_1", "design_2")
  
  
  out[(1:m) %% 2 != 0,] <- sx
  out[(1:m) %% 2 == 0, ]  <- se_rows


  print(out, row.names = FALSE)
  
  
  cat("\n Only showing diagnosands that are statistically different from each other")
  invisible(out)
}



