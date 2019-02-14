#' Compare Designs
#'
#' Diagnose and compare designs.
#'
#' @param design_or_diagnosis1 A design or a diagnosand
#' @param design_or_diagnosis2 A design or a diagnosand
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#' @param add_grouping_variables Variables used to generate groups of simulations for diagnosis. Added to list default list: c("design_label", "estimand_label", "estimator_label", "term")
#' @param sims The number of simulations, defaulting to 500. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{100}. Set to FALSE to turn off bootstrapping.
#' @return a list with a data frame of simulations, a data frame of diagnosands, a vector of diagnosand names, and if calculated, a data frame of bootstrap replicates.
#'
#' # Example w set diagnosands
#'
#' @export
compare_diagnoses <- function(design_or_diagnosis1,
                              design_or_diagnosis2, 
                              sims = 500,
                              bootstrap_sims = 100, 
                              merge_by_estimator = FALSE){
  
  
  if(bootstrap_sims== 0) stop("Please choose a higher number of bootstrap simulations")
  
  
 # Diagnose designs design_or_diagnoses are design object
  if( "design" %in% class(design_or_diagnosis1)  ){
    diagnosis1 = diagnose_design(design_or_diagnosis1, 
                                 sims = sims, 
                                 bootstrap_sims = bootstrap_sims)

    } else if(class(design_or_diagnosis1) == "diagnosis") {
    diagnosis1 <- design_or_diagnosis1} else{ 
    stop("design_or_diagnosis1 must be either a design or a diagnosis")}
  
  
  
  if( "design" %in% class(design_or_diagnosis2)  ){
    diagnosis2 = diagnose_design(design_or_diagnosis2, 
                                 sims = sims, bootstrap_sims = 0)
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
  # at it the largest possible cardinality, merge_by_set contains c("estimand_label", "term", " "estimator_label"") 
  # if group_by_set is different in both designs elements merge_by_set contains elements present in both
  # NOTE: Need to test how comparison is done when only on diagnosis has term or estimator_label
  
  merge_by_set <- c("estimand_label", "term") 
  
  
  if(merge_by_estimator) merge_by_set <- c(merge_by_set, "estimator_label")
  merge_by_set <- base::intersect(merge_by_set,
                    base::intersect(diagnosis1$group_by_set, 
                                        diagnosis2$group_by_set))
  
  if(merge_by_estimator & !"estimator_label" %in% merge_by_set)
    warning("Diagnoses do not include estimator_label")
  
  if(! "estimand_label" %in% merge_by_set) 
    stop("Can't compare diagnoses without estimand_label")
  
  
  comparison_df <-  merge(diagnosis1$diagnosands_df  , diagnosis2$diagnosands_df, 
                          by = merge_by_set, suffixes = c("_1", "_2"))
  
  c_names <- colnames(comparison_df)
  
  
  dropcols <- grepl("[[:digit:]]+$", c_names ) |   c_names  %in% c("design_label", "estimand_label", "estimator_label", "term") 
  
  
  # Grab standard.error columns before droping cols that are not present in both diagnosands_df
  # just in case diagnosis2 doesn't  include bootrstrap_replicates
  filter_se      <- grepl("^se",  c_names)
  diagnosands_se <- comparison_df[,filter_se]
  
  
  comparison_df  <- comparison_df[, dropcols]

  
  # Compute bootstrap confindence interval
  bootstrap_df  <- diagnosis1$bootstrap_replicates
  group_by_list <- bootstrap_df[,  group_by_set, drop = FALSE]
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  bootstrap_df <- split(bootstrap_df , lapply(group_by_list, addNA), drop = TRUE)
  group_by_set <- diagnosis1$group_by_set
  lower.bound <- lapply(bootstrap_df,FUN = function(x) {
    d <- x[, diagnosands]
    set <- head(x[,group_by_set], 1)
    q <- sapply(d, function(d) quantile(d, 0.01, na.rm = TRUE))
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
  filter_mean    <- gsub("_[[:digit:]]+$","", colnames(comparison_df)) %in%  diagnosands
  diagnosands_mean <- comparison_df[,  filter_mean]
  c_names <- colnames(comparison_df)
  design_labels <-  c_names[grepl("^design_label", c_names)]
  estimand_labels <-  c_names[grepl("^estimand_label", c_names)]
  estimator_labels <-  c_names[grepl("^estimator_label", c_names)]
  term_labels <- c_names[grepl("^term", c_names)]
  sims_labels <- c_names[grepl("^n_sims", c_names)]
  sims_df <- comparison_df[, sims_labels]
  comparison_df <- cbind(comparison_df[, design_labels],
                         comparison_df[, estimand_labels], 
                         comparison_df[, term_labels],
                         comparison_df[,  estimator_labels])
 
  
  #prepare output: 
  m <- ncol(diagnosands_mean)
  
  out <- suppressWarnings( do.call(rbind, lapply(1:nrow(diagnosands_mean), function(pair){
    cbind(comparison_df[pair, ], 
          diagnosand = diagnosands,
          t(mapply(function(mean_1, mean_2, se_1, l, u) 
  
            c(mean_1 = mean_1, mean_2 = mean_2, se_1 = se_1, in_interval =  mean_2 >= l & mean_2 <= u, conf.low_1 = l, conf.upper_1 = u), 
            #args
            mean_1 = diagnosands_mean[pair, 1:length(diagnosands)],
            mean_2 = diagnosands_mean[pair, length(diagnosands):ncol(diagnosands_mean)],
            se_1 = diagnosands_se[pair, 1:length(diagnosands)],
            l =  lower.bound[pair, diagnosands],
            u =  upper.bound[pair, diagnosands])),
          sims_df[pair, ])
          
    })))
  
   
    invisible(list(compared.diagnoses_df = as.data.frame(out),
            diagnosis1 = diagnosis1,
            diagnosis2 = diagnosis2))
}




#' @export
print.compared.diagnoses <- function(object, ...) {
  print(summary(object))
  invisible(object)
}


#' @export
summary.compared.diagnoses <- function(x,  ...) {
  x<-  x$compared.diagnoses_df
  sx <- subset(x,  x$in_interval == 0)

  if(nrow(sx) >0) x <- sx
  x<- mutate_if(x, is.numeric, round, digits =2)
  structure(x, class = c("summary.compared_diagnoses", "data.frame"))
  
}



#' @export
print.summary.compared.diagnoses <- function(x, ...){
  print(x, row.names = FALSE)
  invisible(x)
}



