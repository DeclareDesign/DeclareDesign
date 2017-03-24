#' @export
compare_designs <- function(..., sims = 100) {
  designs <- list(...)
  ## check that errybody is a design in ...

  if (!all(sapply(designs, class) == "design")) {
    stop("Please only send design objects to compare_designs.")
  }

  inferred_names <- paste(substitute(list(...)))[-1]
  names(designs)[names(designs) == ""] <- inferred_names[names(designs) == ""]

  comparison_sims <- lapply(designs, diagnose_design, sims = sims)

  simulations_df <- lapply(comparison_sims, function(x)x$simulations) %>% bind_rows(.id = "design_ID")
  diagnosands_df <- lapply(comparison_sims, function(x)x$diagnosands) %>% bind_rows(.id = "design_ID")


  return(list(simulations = simulations_df, diagnosands = diagnosands_df))

}
