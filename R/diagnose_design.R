
#' Diagnose the Design
#'
#' Runs many simulations of a design, and applies a diagnosand function to the results.
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You dican also provide a single list of designs, for example one created by \code{\link{fill_out}}.
#'
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#'
#' @param sims The number of simulations, defaulting to 500.
#' @param bootstrap Number  of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{100}.
#'
#' @details
#'
#' If the diagnosand function contains a \code{group_by} attribute, it will be used to split-apply-combine diagnosands rather than the intersecting column names.
#'
#' If \code{sims} is named, or longer than one element, a fan-out strategy is created and used instead.
#'
#' If the \code{future} package is installed, you can set a \code{\link{future}{plan}} to run multiple simulations at once.
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
#' my_reveal <- declare_reveal()
#'
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_estimand,
#'                          my_assignment,
#'                          my_reveal,
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
diagnose_design <- function(..., diagnosands = default_diagnosands,
                                 sims = 500, bootstrap = 100) {

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

    if (!all(vapply(designs, inherits, FALSE, "design"))) {
      stop("Please only send design objects to diagnose_design.")
    }

    if (is.null(names(designs))) {
      names(designs) <- inferred_names
    } else {
      names(designs)[names(designs) == ""] <- inferred_names[names(designs) == ""]
    }

    if(length(designs) == 1) {
      out <- diagnose_design_single_design(designs[[1]], diagnosands, sims, bootstrap)
      return(out)
    }

    comparison_sims <- lapply(designs,
                              diagnose_design_single_design,
                              diagnosands = diagnosands,
                              sims = sims,
                              bootstrap = bootstrap)



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


    structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    )

  }

diagnose_design_single_design <- function(design, diagnosands, sims, bootstrap) {




    if(length(sims) == 1 && is.null(names(sims))) {
      results_list <- future_lapply(seq_len(sims),
                                    function(i) conduct_design(design),
                                    future.seed = NA, future.globals = "design")
    } else {
      sims <- check_sims(design, sims)
      results_list <- fan_out(design, sims)
      fan_id <- setNames(rev(do.call(expand.grid, lapply(rev(sims$n), seq))), paste0("fan_", seq_len(nrow(sims))))
      fan_id$sim_ID <- seq_len(nrow(fan_id))
    }



    results2x <- function(results_list, what) {
      subresult <- lapply(results_list, `[[`, what)
      df <- do.call(rbind.data.frame, subresult)
      if(nrow(df) == 0) return(df)

      df <- cbind(sim_ID = rep(seq_along(subresult), vapply(subresult, nrow, 0L)), df)
    }


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
          by = colnames(estimands_df) %i% colnames(estimates_df),
          all = TRUE,
          sort = FALSE
        )

      if (nrow(simulations_df) > max(nrow(estimands_df), nrow(estimates_df))) {
        warning("Estimators lack estimand/coefficient labels for matching, a many-to-many merge was performed.")
      }

    }

    if(exists("fan_id")){
      simulations_df <- merge(simulations_df, fan_id, by="sim_ID")
    }



    group_by_set <- attr(diagnosands, "group_by") %||%
      colnames(simulations_df) %i% c("estimand_label", "estimator_label", "coefficient")


    calculate_diagnosands <- function(simulations_df, diagnosands) {
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

    if (bootstrap > 0) {
      boot_indicies_by_id <- split(1:nrow(simulations_df), simulations_df$sim_ID)
      nsims <- max(simulations_df$sim_ID)
      boot_function <- function() {
        boot_ids <- sample.int(nsims, nsims, TRUE)
        boot_indicies <- unlist(boot_indicies_by_id[boot_ids])
        calculate_diagnosands(simulations_df[boot_indicies, , drop=FALSE], diagnosands)
      }

      diagnosand_replicates <- future_lapply(seq_len(bootstrap),
                                             function(i) boot_function(), # this pattern seems to make the NSE happy for now
                                             future.seed = NA,
                                             future.globals="boot_function")
      diagnosand_replicates <- do.call(rbind.data.frame, diagnosand_replicates)


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

    if (nrow(estimates_df) > 0) {

      estimator_f <- factor(diagnosands_df$estimator_label, unique(estimates_df$estimator_label))
      diagnosands_df <- diagnosands_df[order(estimator_f), , drop=FALSE]
    }


    structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    )

  }

