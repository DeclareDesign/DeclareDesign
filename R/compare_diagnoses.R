

compare_diagnosands <- function(diagnosis1, diagnosis2, match_estimator) {
  
  # Prep and merge
  diagnosands  <- intersect(diagnosis1$diagnosand_names, diagnosis2$diagnosand_names)
  merge_by_set <- c("estimand_label", "term")
  
  if(match_estimator) 
    merge_by_set <- c(merge_by_set, "estimator_label")
  
  merge_by_set <- intersect(merge_by_set,
                            c(diagnosis1$group_by_set, diagnosis2$group_by_set))
  
  if(match_estimator & !"estimator_label" %in% merge_by_set)
    warning("Diagnoses do not include estimator_label")
  
  # drop diagnosands that are not in both diangoses
  comparison_df <-  merge(diagnosis1$diagnosands_df  , diagnosis2$diagnosands_df , 
                          by = merge_by_set, suffixes = c("_1", "_2"))
  dropcols <- grepl("[[:digit:]]+$", 
                    colnames(comparison_df)) | colnames(comparison_df) %in% merge_by_set 
  comparison_df <- comparison_df[, dropcols]
  
  
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
  
  # compare
  
  
  
  #prepare output: 
  suppressWarnings(
    out <- do.call(rbind, lapply(1:nrow(diagnosands_mean), function(pair){
      
      cbind(comparison_df[pair, c(merge_by_set,  est_labels)], 
            diagnosand = diagnosands,
            t(mapply(function(m1, m2, s1, s2, tt, p) 
              c(m1 = m1, m2 = m2, s1 = s1,s2 = s2, t.stat = tt, p.val = p), 
              #args
              m1 = diagnosands_mean[pair, (1:m) %% 2 != 0],
              m2 = diagnosands_mean[pair, (1:m) %% 2 == 0],
              s1 = diagnosands_se[pair,   (1:m) %% 2 != 0],
              s2 = diagnosands_se[pair,   (1:m) %% 2 == 0],
              tt = ttest[pair,] ,
              p = p.value[pair,])),
            comparison_df[pair,sims_labels]
      )}
    ))
  )
  outnames <- colnames(out)
  outnames <- gsub("m1", paste0(diagnosis1$parameters_df, "_mean"),  outnames) 
  outnames <- gsub("m2", paste0(diagnosis2$parameters_df, "_mean"),  outnames)   
  outnames <- gsub("s1", paste0(diagnosis1$parameters_df, "_se"),  outnames) 
  outnames <- gsub("s2", paste0(diagnosis2$parameters_df, "_se"),  outnames)     
  colnames(out) <- outnames
  class(out) <- "compare_diagnoses"
  out
}




compare_diagnoses <- function(design_or_diagnosis_1,
                              design_or_diagnosis_2, 
                              sims = 500,
                              bootstrap_sims = 100, 
                              match_estimator = FALSE, 
                              diagnosands = NULL){
  # Send warning when bootrap = 0 or FALSE
  
  if(class(design_or_diagnosis_1) == "design" )
    diagnosis1 = diagnose_design(design_or_diagnosis_1, sims = sims, bootstrap_sims = bootstrap_sims)
  
  else if(class(design_or_diagnosis_1) == "diagnosis")
    diagnosis1 <- design_or_diagnosis_1
  
  else stop("design_or_diagnosis_1 must be either a design or a diagnosis")
  
  if(class(design_or_diagnosis_2) == "design" )
    diagnosis2 = diagnose_design(design_or_diagnosis_2, sims = sims, bootstrap_sims = bootstrap_sims)
  
  else if(class(design_or_diagnosis21) == "diagnosis")
    diagnosis2 <- design_or_diagnosis_2
  
  else stop("design_or_diagnosis_2 must be either a design or a diagnosis")
  compare_diagnosands(diagnosis1, diagnosis2, match_estimator) 
  
}
