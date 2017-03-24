

#' @importFrom dplyr bind_rows group_by left_join
#' @importFrom parallel parLapply
#' @importFrom foreach registerDoSEQ getDoParWorkers
#' @export
diagnose_design <- function(design, diagnosands = default_diagnosands, sims = 100){

  ##if(getDoParWorkers() == 1){
  ##  registerDoSEQ()
  ##}

  ##sims <- parLapply(1:sims, function(x) design$design_function())

  ##sims <- foreach(i = 1:sims) %dorng% {
  ##  design$design_function()
  ##  }

  sims <- lapply(1:sims, function(x) design$design_function())

  estimates_df <-
    lapply(sims, function(x) x$estimates_df) %>% bind_rows(.id = "sim_ID")
  estimands_df <-
    lapply(sims, function(x) x$estimands_df) %>% bind_rows(.id = "sim_ID")

  simulations_df <- suppressMessages(left_join(estimands_df, estimates_df))

  diagnosands_df <-
    simulations_df %>%
    group_by(estimand_label, estimator_label) %>%
    diagnosands %>%
    data.frame(stringsAsFactors = FALSE)

  return(list(simulations = simulations_df, diagnosands = diagnosands_df))

}

