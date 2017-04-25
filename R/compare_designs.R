#' Compare the Properties of Several Research Designs
#'
#' @param ... A set of designs created by \code{\link{declare_design}}.
#'
#' @param sims The number of simulations, defaulting to 500.
#'
#' @importFrom dplyr '%>%'
#' @export
compare_designs <- function(..., sims = 500) {
  designs <- list(...)

  if (!all(sapply(designs, class) == "design")) {
    stop("Please only send design objects to compare_designs.")
  }

  inferred_names <- paste(substitute(list(...)))[-1]
  names(designs)[names(designs) == ""] <-
    inferred_names[names(designs) == ""]

  comparison_sims <- lapply(designs, diagnose_design, sims = sims)

  simulations_df <-
    lapply(comparison_sims, function(x)
      x$simulations) %>% bind_rows(.id = "design_ID")
  diagnosands_df <-
    lapply(comparison_sims, function(x)
      x$diagnosands) %>% bind_rows(.id = "design_ID")

  return(structure(
    list(simulations = simulations_df, diagnosands = diagnosands_df),
    class = "diagnosis"
  ))

}
