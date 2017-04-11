


#' @importFrom dplyr bind_rows group_by_ left_join summarize_
#' @export
diagnose_design <-
  function(design,
           diagnosands = default_diagnosands,
           sims = 100) {
    ##if(getDoParWorkers() == 1){
    ##  registerDoSEQ()
    ##}

    ##sims <- parLapply(1:sims, function(x) design$design_function())

    ##pb <- txtProgressBar(min = 0, max = sims, initial = 0,
    ##                     char = "=", width = 20, style = 3)

    # importFrom foreach registerDoSEQ getDoParWorkers
    # importFrom tcltk tkProgressBar setTkProgressBar
    # importFrom utils txtProgressBar setTxtProgressBar
    # importFrom doRNG %dorng%

    ##sims <- foreach(i = 1:sims) %do% {
    ##  info <- sprintf("%d%% done", round(i/sims))
    ##  setTxtProgressBar(pb, i, sprintf("test (%s)", info), info)
    ##  design$design_function()
    ##}

    sims <- lapply(1:sims, function(x)
      design$design_function())

    estimates_df <-
      lapply(sims, function(x)
        x$estimates_df) %>% bind_rows(.id = "sim_ID")
    estimands_df <-
      lapply(sims, function(x)
        x$estimands_df) %>% bind_rows(.id = "sim_ID")

    simulations_df <-
      suppressMessages(left_join(estimands_df, estimates_df))

    diagnosands_df <-
      simulations_df %>%
      group_by_("estimand_label", "estimator_label") %>%
      diagnosands %>%
      data.frame(stringsAsFactors = FALSE)

    return(structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    ))

  }


#' @export
print.diagnosis <- function(x, ...) {
  print(summary(x))
  invisible(summary(x))
}

#' @export
summary.diagnosis <- function(object, ...) {
  structure(object$diagnosands,
            class = c("summary.diagnosis", "data.frame"))
}

#' @export
print.summary.diagnosis <- function(x, ...) {
  class(x) <- "data.frame"
  cat("\nResearch design diagnosis\n\n")
  print_diagnosis <- x
  names(x) <-
    gsub("(^|[[:space:]])([[:alpha:]])",
         "\\1\\U\\2",
         gsub("_", " ", names(x)),
         perl = TRUE)
  print(x, row.names = FALSE)
  cat("\n")
  invisible(x)
}
