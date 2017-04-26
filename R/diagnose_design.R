


#' Diagnose the Design
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{quick_design}}.
#'
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#'
#' @param sims The number of simulations, defaulting to 500.
#'
#' @examples
#' #' my_population <- declare_population(N = 500, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = noise, Y_Z_1 = noise +
#'   rnorm(N, mean = 2, sd = 2))
#'
#' my_sampling <- declare_sampling(n = 250)
#'
#' my_assignment <- declare_assignment(m = 25)
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_sampling,
#'                          my_estimand,
#'                          dplyr::mutate(noise_sq = noise^2),
#'                          my_assignment,
#'                          reveal_outcomes,
#'                          my_estimator)
#'
#' diagnosis <- diagnose_design(design)
#'
#' @importFrom dplyr bind_rows group_by_ left_join summarize_ '%>%'
#' @export
diagnose_design <- function(..., diagnosands = default_diagnosands, sims = 500) {

  designs <- list(...)

  ## three cases:
  ## 1. send one or more design objects created by declare_design
  ## 2. send a single list of design objects created by quick_design
  ## 3. do not allow sending more than one object if any of them aren't design objects.
  if (length(designs) == 1 & class(designs[[1]]) == "list") {
    designs <- designs[[1]]
  } else if (!all(sapply(designs, class) == "design")) {
    stop("Please only send design objects to diagnose_design.")
  }

  inferred_names <- paste(substitute(list(...)))[-1]
  names(designs)[names(designs) == ""] <-
    inferred_names[names(designs) == ""]

  comparison_sims <- lapply(designs, diagnose_design_single_design,
                            diagnosands = diagnosands, sims = sims)

  if (length(comparison_sims) > 1) {
    simulations_df <-
      lapply(comparison_sims, function(x)
        x$simulations) %>% bind_rows(.id = "design_ID")
    diagnosands_df <-
      lapply(comparison_sims, function(x)
        x$diagnosands) %>% bind_rows(.id = "design_ID")
  } else {
    simulations_df <- comparison_sims[[1]]$simulations
    diagnosands_df <- comparison_sims[[1]]$diagnosands
  }

  return(structure(
    list(simulations = simulations_df, diagnosands = diagnosands_df),
    class = "diagnosis"
  ))

}

#' @export
get_diagnosands <- function(diagnosis){
  diagnosis$diagnosands
}

#' @export
get_simulations <- function(diagnosis){
  diagnosis$simulations
}


diagnose_design_single_design <-
  function(design,
           diagnosands = default_diagnosands,
           sims = 500) {
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

    if (nrow(simulations_df) == 0 & nrow(estimates_df) > 1) {
      simulations_df <- estimates_df
    }


    group_by_set <- c("estimand_label", "estimator_label")[
      which(c("estimand_label", "estimator_label") %in% colnames(simulations_df))]

    diagnosands_df <-
      simulations_df %>%
      group_by_(.dots = group_by_set) %>%
      diagnosands %>%
      data.frame(stringsAsFactors = FALSE)

    characteristics <- design$characteristics

    if (!is.null(characteristics)) {
      for(i in seq_along(characteristics)){
        simulations_df[,names(characteristics)[i]] <- characteristics[i]
        diagnosands_df[,names(characteristics)[i]] <- characteristics[i]
      }
    }


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


