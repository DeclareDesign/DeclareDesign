

#' @export
compare_diagnoses <- function(design_or_diagnosis1,
                              design_or_diagnosis2, 
                              sims = 500,
                              bootstrap_sims = 100, 
                              diagnosands = NULL,
                              add_grouping_variables = NULL ,
                              match_estimator = FALSE, ...){
  # Send warning when bootrap = 0 or FAL
  if(bootstrap_sims== 0) stop("Please choose a higher number of bootstrap simulations")
  
  
  if(class(design_or_diagnosis1) == "design" ){
    diagnosis1 = diagnose_design(diagnosis1, 
                                 sims = sims, 
                                 bootstrap_sims = bootstrap_sims
    )}
  
  else if(class(design_or_diagnosis1) == "diagnosis") {
    diagnosis1 <- design_or_diagnosis1}
  
  else{ 
    stop("design_or_diagnosis1 must be either a design or a diagnosis")}
  
  
  if(class(design_or_diagnosis2) == "design" ){
    diagnosis2 = diagnose_design(diagnosis2, sims = sims, bootstrap_sims = bootstrap_sims)}
  
  else if(class(design_or_diagnosis2) == "diagnosis") {
    diagnosis2 <- design_or_diagnosis2}
  
  else {
    stop("design_or_diagnosis2 must be either a design or a diagnosis")}
  compare_diagnoses_internal(diagnosis1, diagnosis2, match_estimator) 
  
}



compare_diagnoses_internal <- function(diagnosis1, diagnosis2, match_estimator ) {
  
  # Housekeeping
  if(class(diagnosis1) != "diagnosis" | class(diagnosis2) != "diagnosis" ) 
    stop("Can't compare designs without diagnoses ")
  if(is.null(diagnosis1$bootstrap_replicates ) | is.null(diagnosis2$bootstrap_replicates )) 
    stop("Can't compare diagnoses witouth bootstrap replicates")
  
  
  # Prep and merge
  diagnosands  <- intersect(diagnosis1$diagnosand_names, diagnosis2$diagnosand_names)
  merge_by_set <- c("estimand_label", "term")
  if(match_estimator) merge_by_set <- c(merge_by_set, "estimator_label")
  merge_by_set <- intersect(merge_by_set,
                            c(diagnosis1$group_by_set, diagnosis2$group_by_set))
  
  if(match_estimator & !"estimator_label" %in% merge_by_set)
    warning("Diagnoses do not include estimator_label")
  
  comparison_df <-  merge(diagnosis1$diagnosands_df  , diagnosis2$diagnosands_df , 
                          by = merge_by_set, suffixes = c("_1", "_2"))
  
  dropcols <- grepl("[[:digit:]]+$", 
                    colnames(comparison_df)) | colnames(comparison_df) %in% merge_by_set 
  
  comparison_df  <- comparison_df[, dropcols]
  
  
  # Order merged df so that every other column is from diag1 and every other is from diag2
  a <- length(merge_by_set)
  b <- ncol(comparison_df)
  ab <- (b - a)/2
  i <- c(mapply(function(x, y) c(x,y), (a + 1):(a + ab), (a + ab + 1):b))
  comparison_df <- comparison_df[, c(1:a, i)]
  
  # split columns by se and factors
  filter_se      <- grepl("^se", colnames(comparison_df))
  filter_mean    <- gsub("_[[:digit:]]+$","", colnames(comparison_df)) %in%  diagnosands
  diagnosands_se <- comparison_df[,filter_se]
  diagnosands_mean <- comparison_df[,  filter_mean]
  c_names <- colnames(comparison_df)
  est_labels <-  c_names[grepl("^estimator_label", c_names)]
  sims_labels <- c_names[grepl("^n_sims", c_names)]
  
  #prepare output: 
  m <- ncol(diagnosands_mean)
  out <- suppressWarnings( do.call(rbind, lapply(1:nrow(diagnosands_mean), function(pair){
    
    cbind(comparison_df[pair, c(merge_by_set,  est_labels)], 
          diagnosand = diagnosands,
          t(mapply(function(m1, m2, s1, s2) 
            c(m1 = m1, m2 = m2, s1 = s1,s2 = s2, 
              divergence = abs(m1 - m2)> 2*s1), 
            #args
            m1 = diagnosands_mean[pair, (1:m) %% 2 != 0],
            m2 = diagnosands_mean[pair, (1:m) %% 2 == 0],
            s1 = diagnosands_se[pair,   (1:m) %% 2 != 0],
            s2 = diagnosands_se[pair,   (1:m) %% 2 == 0])),
          comparison_df[pair,sims_labels]
          
    )})))
  
  
  outnames <- colnames(out)
  outnames <- gsub("m1", paste0(diagnosis1$parameters_df, "_mean"),  outnames) 
  outnames <- gsub("m2", paste0(diagnosis2$parameters_df, "_mean"),  outnames)   
  outnames <- gsub("s1", paste0(diagnosis1$parameters_df, "_se"),  outnames) 
  outnames <- gsub("s2", paste0(diagnosis2$parameters_df, "_se"),  outnames)     
  colnames(out) <- outnames
  class(out) <- "diagnoses_comparison"
  out
}




