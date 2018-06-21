
#' Diagnose the design
#'
#' Generates diagnosands from a design or simulations of a design. 
#'
#' @param ... A design or set of designs typically created using the + operator, or a \code{data.frame} of simulations, typically created by \code{\link{simulate_design}}.
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#' @param add_grouping_variables Variables used to generate groups of simulations for diagnosis. Added to list default list: c("design_label", "estimand_label", "estimator_label", "coefficient")
#' @param sims The number of simulations, defaulting to 500. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}
#' @param bootstrap_sims Number of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{100}. Set to FALSE to turn off bootstrapping.
#' @return a list with a data frame of simulations, a data frame of diagnosands, a vector of diagnosand names, and if calculated, a data frame of bootstrap replicates.
#'
#'
#' @details
#'
#' If the diagnosand function contains a \code{group_by} attribute, it will be used to split-apply-combine diagnosands rather than the intersecting column names.
#'
#' If \code{sims} is named, or longer than one element, a fan-out strategy is created and used instead.
#'
#' If the \code{future} package is installed, you can set  \code{\link[future]{plan}} to run multiple simulations at once.
#'
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
#' my_reveal <- declare_reveal()
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' design <- my_population +
#'    my_potential_outcomes +
#'    my_estimand +
#'    my_assignment +
#'    my_reveal +
#'    my_estimator
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
#' # Using an existing data frame of simulations
#' \dontrun{
#' simulations <- simulate_design(designs, sims = 2)
#' diagnosis   <- diagnose_design(simulations_df = simulations_df)
#'
#' }

#' @importFrom stats setNames
#' @importFrom utils head
#' @export 
diagnose_design <- function(...,
                            diagnosands = default_diagnosands,
                            sims = 500,
                            bootstrap_sims = 100,
                            add_grouping_variables = NULL) {
    
  dots_quos <- quos(...)
  dots_quos <- maybe_add_labels(dots_quos)
  dots <- lapply(dots_quos, eval_tidy)
  
  # three cases:
  # 1. it's a data frame -- this is the simulations df
  # 2. it's a single object that is a list of designs -- unpack designs
  # 3. it's a ... of one or more designs
  if (length(dots) == 1 && inherits(dots[[1]], "data.frame")) {
    simulations_df <- dots[[1]]
    if (!any(names(simulations_df) %in% c("estimator_label", "estimand_label"))) {
      stop("Can't calculate diagnosands on this data.frame, which does not include either an estimator_label or an estimand_label. Did you send a simulations data frame?")
    }
  } else {
    if (inherits(dots[[1]], "list")) {
      
      if(length(dots) > 1){
        stop("When you provide a list of designs to `diagnose_design`, for example created by `expand_design`, please don't also provide additional designs. Only send the list of designs.")
      }
      designs <- dots[[1]]
      dots_quos <- dots_quos[[1]]
    } else {
      designs <- dots
    }
    
    check_design_class(designs)
  }

  # simulate if needed ------------------------------------------------------

  if (!exists("simulations_df")) {
    simulations_df <- simulate_design(!!!dots_quos, sims = sims)
  }

  # figure out what to group by ---------------------------------------------

  group_by_set <- c("design_label", "estimand_label", "estimator_label", "coefficient")

  if (!is.null(add_grouping_variables)) {
    group_by_set <- c(group_by_set, add_grouping_variables)
  }

  group_by_set <- group_by_set %i% colnames(simulations_df)

  # Actually calculate diagnosands ------------------------------------------

  diagnosands_df <- calculate_diagnosands(simulations_df = simulations_df,
                                          diagnosands = diagnosands,
                                          group_by_set = group_by_set)

  diagnosand_names <- setdiff(names(diagnosands_df), group_by_set)

  # Calculate n_sims --------------------------------------------------------

  n_sims_df <- calculate_sims(simulations_df = simulations_df, group_by_set = group_by_set)    

  # Bootstrap ---------------------------------------------------------------

  if (bootstrap_sims > 0) {
    bootout <- bootstrap_diagnosands(bootstrap_sims = bootstrap_sims,
                                     diagnosands = diagnosands,
                                     simulations_df = simulations_df,
                                     diagnosands_df = diagnosands_df,
                                     group_by_set = group_by_set)
    diagnosands_df <- bootout$diagnosands_df
  }

  # prep for return ---------------------------------------------------------
  
  diagnosands_df <- merge(x = diagnosands_df, y = n_sims_df, by = group_by_set, all = TRUE)

  parameters_df <- attr(simulations_df, "parameters")
  diagnosands_df <- merge(diagnosands_df, parameters_df, by = "design_label")
  
  # make design_label a factor
  diagnosands_df$design_label <- factor(diagnosands_df$design_label, levels = parameters_df$design_label)
  
  # Reorder rows
  sort_by_list <- c(group_by_set, "statistic") %i% colnames(diagnosands_df) 
  diagnosands_df <- diagnosands_df[do.call(order, as.list(diagnosands_df[sort_by_list])), , drop = FALSE]

  rownames(diagnosands_df) <- NULL
  # Return frames
  out <- list(simulations_df = simulations_df, diagnosands_df = diagnosands_df, diagnosand_names = diagnosand_names, group_by_set = group_by_set, parameters_df = parameters_df)
  
  if (bootstrap_sims != 0) {
    out$bootstrap_replicates <- bootout$diagnosand_replicates
  }
  out$bootstrap_sims <- bootstrap_sims

  structure(out, class = "diagnosis")

}

check_design_class <- function(designs){
  if (!all(sapply(designs, function(x) {
    inherits(x, "design") || ( inherits(x, "function") && is.null(formals(x)))
  }))) {
    stop("Please only send design objects or functions with no arguments to simulate_design.")
  }
}

calculate_diagnosands <- function(simulations_df, diagnosands, group_by_set) {
  group_by_list <- simulations_df[, group_by_set, drop = FALSE]
  
  labels_df <- split(group_by_list, group_by_list, drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  
  diagnosands_df <- split(simulations_df, group_by_list, drop = TRUE)
  diagnosands_df <- lapply(diagnosands_df, FUN = function(x) {   
    dg <- diagnosands(x)  
    setNames(dg[[2]], dg[[1]]) 
  })
  
  # c appropriate and fast here bc labels_df must be a list (drop=FALSE above)
  diagnosands_df <- as.data.frame(t(mapply(c, labels_df, diagnosands_df)), stringsAsFactors = FALSE)
  
  diagnosands_df[] <- lapply(diagnosands_df, unlist)
  
  diagnosands_df
}

calculate_sims <- function(simulations_df, group_by_set) {
  group_by_list <- simulations_df[, group_by_set, drop = FALSE]
  
  labels_df <- split(group_by_list, group_by_list, drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  
  n_sims_df <- split(simulations_df, group_by_list, drop = TRUE)
  n_sims_df <- lapply(n_sims_df, FUN = function(x) {   
    data.frame(n_sims = nrow(x))
  })
  
  # c appropriate and fast here bc labels_df must be a list (drop=FALSE above)
  n_sims_df <- as.data.frame(t(mapply(c, labels_df, n_sims_df)), stringsAsFactors = FALSE)
  n_sims_df[] <- lapply(n_sims_df, unlist)
  
  n_sims_df
}



bootstrap_diagnosands <- function(bootstrap_sims, simulations_df, diagnosands, diagnosands_df, group_by_set) {
  
  boot_indicies_by_id <- split(1:nrow(simulations_df), simulations_df$sim_ID)
  nsims <- max(simulations_df$sim_ID)
  
  boot_function <- function() {
    boot_ids      <- sample.int(nsims, nsims, TRUE)
    boot_indicies <- unlist(boot_indicies_by_id[boot_ids])
    calculate_diagnosands(simulations_df = simulations_df[boot_indicies, , drop = FALSE],
                          diagnosands = diagnosands,
                          group_by_set = group_by_set)
  }
  
  # Make a list of diagnosand replicates
  diagnosand_replicates <- future_lapply(seq_len(bootstrap_sims),
                                         function(i) cbind(bootstrap_id = i, boot_function()), # Add boostrap ID
                                         future.seed = NA,
                                         future.globals = "boot_function")
  
  # Make a data.frame of diagnosand replicates
  diagnosand_replicates <- do.call(rbind.data.frame, diagnosand_replicates)
  rownames(diagnosand_replicates)  <- NULL
  
  # Prep for se calculation
  group_by_list <- diagnosand_replicates[, group_by_set, drop = FALSE]
  labels_df <- split(group_by_list, group_by_list, drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)
  
  diagnosands_df_group <- diagnosands_df[, group_by_set, drop = FALSE]
  diagnosands_df[group_by_set] <- NULL
  diagnosands_names <- colnames(diagnosands_df)
  
  diagnosands_df <- split(diagnosands_df, diagnosands_df_group, drop = TRUE)
  diagnosands_df <- diagnosands_df[names(labels_df)]
  
  # Calculate standard errors
  use_vars <- names(diagnosand_replicates)[!(names(diagnosand_replicates) %in% c(group_by_set, "bootstrap_id"))]
  diagnosands_se_df <- split(diagnosand_replicates[use_vars], group_by_list, drop = TRUE)
  diagnosands_se_df <- lapply(diagnosands_se_df, lapply, sd)
  
  # Clean up
  diagnosands_se_df <- lapply(diagnosands_se_df, setNames, sprintf("se(%s)", diagnosands_names))
  
  diagnosands_df <- as.data.frame(t(mapply(c, labels_df, diagnosands_df, diagnosands_se_df)),
                                  stringsAsFactors = FALSE)
  diagnosands_df[] <- lapply(diagnosands_df, unlist)
  
  # Permute columns so SEs are right of diagnosands
  n_diag <- length(diagnosands_names)
  i <- c(seq_along(group_by_set),
         length(group_by_set) + rep(seq_len(n_diag), each = 2) + c(0, n_diag))
  diagnosands_df <- diagnosands_df[, i, drop = FALSE]
  
  # Reordering columns
  dim_cols <- c("estimator_label", "coefficient", "estimand_label") %i% group_by_set
  ix <- sort(match(dim_cols, colnames(diagnosands_df)))
  diagnosands_df[ix] <- diagnosands_df[dim_cols]
  names(diagnosands_df)[ix] <- dim_cols
  
  list(diagnosands_df = diagnosands_df, 
       diagnosand_replicates = diagnosand_replicates)
}
