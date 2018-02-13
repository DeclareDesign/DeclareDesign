
#' Diagnose the Design
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You dican also provide a single list of designs, for example one created by \code{\link{fill_out}}.
#'
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#'
#' @param sims The number of simulations, defaulting to 500.
#' @param bootstrap Option to bootstrap the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{TRUE}.
#' @param bootstrap_sims The number of bootstrap replicates of the diagnosands, defaulting to 100.
#' @param parallel Logical indicating whether to run the diagnoses in parallel. Defaults to TRUE.
#' @param parallel_cores Number of CPU cores to use. Defaults to all available cores.
#'
#' @examples
#' my_population <- declare_population(N = 500, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = noise, Y_Z_1 = noise +
#'   rnorm(N, mean = 2, sd = 2))
#'
#' my_assignment <- declare_assignment()
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_estimand,
#'                          my_assignment,
#'                          reveal_outcomes,
#'                          my_estimator)
#'
#' \dontrun{
#' # using built-in defaults:
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' }
#'
#' # using a user-defined diagnosand
#' my_diagnosand <- declare_diagnosands(absolute_error = mean(abs(est - estimand)))
#'
#' \dontrun{
#' diagnosis <- diagnose_design(design, diagnosands = my_diagnosand)
#' diagnosis
#'
#' get_diagnosands(diagnosis)
#'
#' get_simulations(diagnosis)
#'
#' }
#'
#' @importFrom stats setNames
#' @importFrom utils head
#' @export
diagnose_design <-
  function(...,
           diagnosands = default_diagnosands,
           sims = 500,
           bootstrap = TRUE,
           bootstrap_sims = 100,
           parallel = TRUE,
           parallel_cores = detectCores(logical = TRUE)) {

    designs <- list(...)

    inferred_names <- paste(substitute(list(...)))[-1]

    ## three cases:
    ## 1. send one or more design objects created by declare_design
    ## 2. send a single list of design objects created by fill_out
    ## 3. do not allow sending more than one object if any of them aren't design objects.
    if (length(designs) == 1 && is.list(designs[[1]]) && !"design" %in% class(designs[[1]]) ) {
      ## this unpacks designs if a list of designs was sent as a single list object, i.e.
      ##   as created by fill_out
      designs <- designs[[1]]
      if (!is.null(names(designs))) {
        inferred_names <- names(designs)
      } else {
        inferred_names <- paste0("design_", 1:length(designs))
      }
    }

    if (!all(sapply(designs, class) == "design")) {
      stop("Please only send design objects to diagnose_design.")
    }

    if (is.null(names(designs))) {
      names(designs) <- inferred_names
    } else{
      names(designs)[names(designs) == ""] <-
        inferred_names[names(designs) == ""]
    }

    comparison_sims <- lapply(designs,
                              diagnose_design_single_design,
                              diagnosands = diagnosands,
                              sims = sims,
                              bootstrap = bootstrap,
                              bootstrap_sims = bootstrap_sims,
                              parallel = parallel,
                              parallel_cores = parallel_cores)

    if (length(comparison_sims) > 1) {

      simulations_list <- lapply(comparison_sims, function(x) x$simulations)
      diagnosands_list <- lapply(comparison_sims, function(x) x$diagnosands)

      for (i in 1:length(simulations_list)) {
        simulations_list[[i]] <- cbind(design_ID = names(simulations_list)[i], simulations_list[[i]])
        diagnosands_list[[i]] <- cbind(design_ID = names(diagnosands_list)[i], diagnosands_list[[i]])
      }

      simulations_df <- do.call(rbind, simulations_list)
      diagnosands_df <- do.call(rbind, diagnosands_list)

      rownames(simulations_df) <- NULL
      rownames(diagnosands_df) <- NULL

    } else {
      simulations_df <- comparison_sims[[1]]$simulations
      diagnosands_df <- comparison_sims[[1]]$diagnosands
    }

    return(structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    ))

  }

#' @importFrom rlang !! !!!
#' @importFrom foreach registerDoSEQ getDoParWorkers foreach %dopar%
#' @importFrom doRNG %dorng%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
diagnose_design_single_design <-
  function(design,
           diagnosands = NULL,
           sims = 500,
           bootstrap = TRUE,
           bootstrap_sims = 100,
           parallel = TRUE,
           parallel_cores = detectCores(logical = TRUE)) {

    if (parallel) {
      registerDoParallel(cores = parallel_cores)
    } else {
      registerDoSEQ()
    }

    results_list <- foreach(i = seq_len(sims)) %dorng% execute_design(design)

    results2x <- function(results_list, what) {
      subresult <- lapply(results_list, `[[`, what)
      df <- do.call(rbind.data.frame, subresult)
      if(nrow(df) == 0) return(df)

      df <- cbind(sim_ID = rep(1:sims, sapply(subresult, nrow)), df)
    }


    # estimates_list <- lapply(results_list, function(x) x$estimates_df)
    # estimates_df <- do.call(rbind, estimates_list)
    # estimates_df <- cbind(sim_ID = rep(1:sims, sapply(estimates_list, nrow)), estimates_df)
    #
    # estimands_list <- lapply(results_list, function(x) x$estimands_df)
    # estimands_df <- do.call(rbind, estimands_list)
    # estimands_df <- cbind(sim_ID = rep(1:sims, sapply(estimands_list, nrow)), estimands_df)

    estimates_df <- results2x(results_list, "estimates_df")
    estimands_df <- results2x(results_list, "estimands_df")
    if (nrow(estimates_df) == 0 & nrow(estimands_df) == 0) {
      stop("No estimates or estimands were declared, so diagnose_design cannot calculate diagnosands.", call. = FALSE)
    }

    if (nrow(estimands_df) == 0 && nrow(estimates_df) > 0) {
      simulations_df <- estimates_df
    } else if (nrow(estimands_df) > 0 && nrow(estimates_df) == 0) {
      simulations_df <- estimands_df
    } else {
      simulations_df <-
        merge(
          estimands_df,
          estimates_df,
          by = c("sim_ID",
                 "estimand_label" %i% colnames(estimates_df),
                 "coefficient_name" %i% colnames(estimands_df) %i% colnames(estimates_df)),
          all = TRUE,
          sort = FALSE
        )

      if(nrow(simulations_df) > max(nrow(estimands_df), nrow(estimates_df))){

        warning("Estimators lack estimand/coefficient labels for matching, a many-to-many merge was performed.")

      }

    }

    calculate_diagnosands <-
      function(simulations_df, diagnosands){
      group_by_set <- colnames(simulations_df) %i% c("estimand_label", "estimator_label", "coefficient_name")
      group_by_list <- simulations_df[, group_by_set, drop=FALSE]

      labels_df <- split(group_by_list, group_by_list, drop = TRUE)
      labels_df <- lapply(labels_df, head, n=1)

      diagnosands_df <- split(simulations_df, group_by_list, drop=TRUE)
      diagnosands_df <- lapply(diagnosands_df, FUN = function(x){ dg <- diagnosands(x); setNames(dg[[2]], dg[[1]]) })

      diagnosands_df <- as.data.frame(t(mapply(c, labels_df, diagnosands_df)), stringsAsFactors=FALSE) # c appropriate and fast here bc labels_df must be a list (drop=FALSE above)
      diagnosands_df[] <- lapply(diagnosands_df, unlist)

      # diagnosands_df <- merge(labels_df, diagnosands_df, by = "row.names", all = TRUE, sort = FALSE)
      # diagnosands_df$Row.names <- NULL
      return(diagnosands_df)
    }

    diagnosands_df <- calculate_diagnosands(simulations_df, diagnosands)

    if (bootstrap) {
      boot_indicies_by_id <- split(1:nrow(simulations_df), simulations_df$sim_ID)
      boot_function <- function() {
        boot_ids <- sample.int(sims, sims, TRUE)
        boot_indicies <- unlist(boot_indicies_by_id[boot_ids])
        calculate_diagnosands(simulations_df[boot_indicies, , drop=FALSE], diagnosands)
      }

      diagnosand_replicates <- foreach(i = seq_len(bootstrap_sims), .combine = rbind) %dorng%
        boot_function()

      # diagnosand_replicates <-
      #   replicate(bootstrap_sims, expr = boot_function(), simplify = FALSE)
      # diagnosand_replicates <- do.call(rbind, diagnosand_replicates)

      group_by_set <-  colnames(diagnosand_replicates) %i% c("estimand_label", "estimator_label", "coefficient_name")
      group_by_list <- diagnosand_replicates[, group_by_set, drop=FALSE]

      labels_df <- split(group_by_list, group_by_list, drop=TRUE)
      labels_df <- lapply(labels_df, head, n=1)

      diagnosands_df_group <- diagnosands_df[, group_by_set, drop=FALSE]
      diagnosands_df[group_by_set] <- NULL
      diagnosands_names <- colnames(diagnosands_df)
      diagnosands_df <- split(diagnosands_df, diagnosands_df_group, drop=TRUE)
      diagnosands_df <- diagnosands_df[names(labels_df)]

      diagnosand_replicates[group_by_set] <- NULL

      diagnosands_se_df <- split(diagnosand_replicates, group_by_list, drop = TRUE)
      diagnosands_se_df <- lapply(diagnosands_se_df, lapply, sd)

      diagnosands_se_df <- lapply(diagnosands_se_df, setNames, sprintf("se(%s)", diagnosands_names))

      diagnosands_df <- as.data.frame(
        t(mapply(c, labels_df, diagnosands_df, diagnosands_se_df )),
        stringsAsFactors=FALSE)
      diagnosands_df[] <- lapply(diagnosands_df, unlist)

      # permute columns so SEs are right of diagnosands
      n_diag <- length(diagnosands_names)
      i <- c(seq_along(group_by_set), length(group_by_set) + rep(seq_len(n_diag), each=2) + c(0, n_diag))
      diagnosands_df <- diagnosands_df[,i, drop=FALSE]


    }

    return(structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    ))

  }

