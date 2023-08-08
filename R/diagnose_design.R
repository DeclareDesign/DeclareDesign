
#' Diagnose the design
#'
#' Generates diagnosands from a design or simulations of a design. Speed gains can be achieved by running diagnose_design in parallel, see Examples.
#'
#' @param ... A design or set of designs typically created using the + operator, or a \code{data.frame} of simulations, typically created by \code{\link{simulate_design}}.
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the inquiry(s).
#' @param make_groups Add group variables within which diagnosand values will be calculated. New variables can be created or variables already in the simulations data frame selected. Type name-value pairs within the function \code{vars}, i.e. \code{vars(significant = p.value <= 0.05)}.
#' @param add_grouping_variables Deprecated. Please use make_groups instead. Variables used to generate groups of simulations for diagnosis. Added to default list: c("design", "estimand_label", "estimator", "outcome", "term")
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
#' If the packages \code{future} and \code{future.apply} are installed, you can set \code{\link[future]{plan}} to run multiple simulations in parallel.
#'
#'
#' @examples
#' 
#' # Two-arm randomized experiment
#' n <- 500
#' 
#' design <-
#'   declare_model(
#'     N = 1000,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = n)) +
#'   declare_assignment(Z = complete_ra(N = N, m = n/2)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' \dontrun{
#' # Diagnose design using default diagnosands
#' diagnosis <- diagnose_design(design)
#' diagnosis
#' 
#' # Use tidy to produce data.frame with bootstrapped standard 
#' # errors and confidence intervals for each diagnosand
#' diagnosis_df <- tidy(diagnosis)
#' diagnosis_df
#' 
#' # Use sims argument to change the number of simulations used 
#' # to calculate diagnosands, and bootstrap_sims to change how
#' # many bootstraps are uses to calculate standard errors.
#' diagnosis <- diagnose_design(design,
#'                              sims = 500,
#'                              bootstrap_sims = 150)
#' tidy(diagnosis)
#' 
#' # You may also run diagnose_design in parallel using 
#' #   the future package on a personal computer with multiple
#' #   cores or on high performance computing clusters. 
#' 
#' library(future)
#' options(parallelly.fork.enable = TRUE) # required for use in RStudio
#' plan(multicore) # note other plans are possible, see future
#' 
#' diagnose_design(design, sims = 500)
#' 
#' # Select specific diagnosands
#' reshape_diagnosis(diagnosis, select = "Power")
#' 
#' # Use your own diagnosands
#' my_diagnosands <-
#'   declare_diagnosands(median_bias = median(estimate - estimand),
#'                       absolute_error = mean(abs(estimate - estimand)))
#' 
#' diagnosis <- diagnose_design(design, diagnosands = my_diagnosands)
#' diagnosis
#' 
#' get_diagnosands(diagnosis)
#' 
#' get_simulations(diagnosis)
#' 
#' # Diagnose using an existing data frame of simulations
#' simulations <- simulate_design(design, sims = 500)
#' diagnosis   <- diagnose_design(simulations_df = simulations)
#' diagnosis
#' 
#' }
#' 
#' # If you do not specify diagnosands, the function default_diagnosands() is used, 
#' #   which is reproduced below.
#' 
#' alpha <- 0.05
#' 
#' default_diagnosands <- 
#'   declare_diagnosands(
#'     mean_estimand = mean(estimand),
#'     mean_estimate = mean(estimate),
#'     bias = mean(estimate - estimand),
#'     sd_estimate = sqrt(pop.var(estimate)),
#'     rmse = sqrt(mean((estimate - estimand) ^ 2)),
#'     power = mean(p.value <= alpha),
#'     coverage = mean(estimand <= conf.high & estimand >= conf.low)
#'   )
#'   
#' diagnose_design(
#'   design, 
#'   diagnosands = default_diagnosands
#' )
#' 
#' # A longer list of useful diagnosands might include:
#' 
#' extended_diagnosands <- 
#'   declare_diagnosands(
#'     mean_estimand = mean(estimand),
#'     mean_estimate = mean(estimate),
#'     bias = mean(estimate - estimand),
#'     sd_estimate = sd(estimate),
#'     rmse = sqrt(mean((estimate - estimand) ^ 2)),
#'     power = mean(p.value <= alpha),
#'     coverage = mean(estimand <= conf.high & estimand >= conf.low),
#'     mean_se = mean(std.error),
#'     type_s_rate = mean((sign(estimate) != sign(estimand))[p.value <= alpha]),
#'     exaggeration_ratio = mean((estimate/estimand)[p.value <= alpha]),
#'     var_estimate = pop.var(estimate),
#'     mean_var_hat = mean(std.error^2),
#'     prop_pos_sig = mean(estimate > 0 & p.value <= alpha),
#'     mean_ci_length = mean(conf.high - conf.low)
#'   )
#'   
#' \dontrun{
#' diagnose_design(
#'   design, 
#'   diagnosands = extended_diagnosands
#' )
#' 
#' # Adding a group for within group diagnosis:
#' diagnosis <- diagnose_design(design, 
#'                              make_groups = vars(significant = p.value <= 0.05),
#' )
#' diagnosis
#' 
#' n <- 500
#' design <-
#'   declare_model(
#'     N = 1000,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ rnorm(1) * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = n)) +
#'   declare_assignment(Z = complete_ra(N = N, m = n/2)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' diagnosis <- diagnose_design(design, 
#'                              make_groups = 
#'                                vars(effect_size = 
#'                                       cut(estimand, quantile(estimand, (0:4)/4), 
#'                                           include.lowest = TRUE)),
#' )
#' diagnosis
#' 
#' # redesign can be used in conjunction with diagnose_designs
#' # to optimize the design for specific diagnosands
#' design_vary_N <- redesign(design, n = c(100, 500, 900))
#' diagnose_designs(design_vary_N)
#' 
#' # Calculate and plot the power of a design over a range of 
#' # effect sizes
#' design <-
#'   declare_model(
#'     N = 200,
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ runif(1, 0.0, 0.5) * Z + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = complete_ra(N)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE") 
#' 
#' library(tidyverse)
#' 
#' simulations_df <- 
#'   diagnose_design(design) |> 
#'   get_simulations() |> 
#'   mutate(significant = if_else(p.value <= 0.05, 1, 0))
#' 
#' ggplot(simulations_df) + 
#'   stat_smooth(
#'     aes(estimand, significant), 
#'     method = 'loess', 
#'     color = "#3564ED", 
#'     fill = "#72B4F3", 
#'     formula = 'y ~ x'
#'   ) +
#'   geom_hline(
#'   yintercept = 0.8, color = "#C6227F", linetype = "dashed") +
#'   annotate("text", x = 0, y = 0.85, 
#'     label = "Conventional power threshold = 0.8", 
#'     hjust = 0, color = "#C6227F") + 
#'   scale_y_continuous(breaks = seq(0, 1, 0.2)) +
#'   coord_cartesian(ylim = c(0, 1)) +
#'   theme(legend.position = "none") +
#'   labs(x = "Model parameter: true effect size",
#'        y = "Diagnosand: statistical power") +
#'   theme_minimal()
#' }
#' 
#' @importFrom stats setNames
#' @importFrom utils head
#' @export
diagnose_design <- function(...,
                            diagnosands = NULL,
                            sims = 500,
                            bootstrap_sims = 100,
                            make_groups = NULL,
                            add_grouping_variables = NULL) {
  
  start_time <- Sys.time()
  
  dots <- quos(...)
  
  if(!is.null(add_grouping_variables)){
    warning("The argument add_grouping_variables is deprecated. Please use make_groups instead.", call. = FALSE)
  }

  # two cases:
  # 1. it's a data frame -- this is the simulations df
  # 2. it's something else, and needs to be simulated
  if (is.data.frame(..1)) {
    simulations_df <- ..1
    if (is_empty(c("estimator", "inquiry") %icn% simulations_df)) {
      stop("Can't calculate diagnosands on this data.frame, which does not include either an estimator or an inquiry. Did you send a simulations data frame?")
    }
    if (nrow(simulations_df) == 0) {
      stop("Can't calculate diagnosands on this data.frame, which has zero rows.")
    }
    diagnosands <- diagnosands %||% attr(simulations_df, "diagnosands") %||% default_diagnosands
  } else {
    # simulate if needed ------------------------------------------------------
    simulations_df <- simulate_design(!!!dots, sims = sims)
    diagnosands <- setup_diagnosands(!!!dots, diagnosands = diagnosands)
  }

  # figure out what to group by ---------------------------------------------

  # Optionally modify the simulations dataframe to create new grouping variables
  if (!is.null(make_groups)) {
    make_groups_names <- names(make_groups)
    make_groups_has_names <- make_groups_names != ""
    make_groups_names[!make_groups_has_names] <-
      sapply(make_groups, as_label)[!make_groups_has_names]
    
    simulations_df <-
      eval_tidy(quo(fabricate(simulations_df,!!!make_groups[make_groups_has_names])))
    
  } else {
    make_groups_names <- NULL
  }

  group_by_set <- 
    c("design", "inquiry", "estimator", "outcome", "term", add_grouping_variables, make_groups_names) %icn% 
    simulations_df
  
  # Actually calculate diagnosands ------------------------------------------

  diagnosands_df <- calculate_diagnosands(
    simulations_df = simulations_df,
    diagnosands = diagnosands,
    group_by_set = group_by_set
  )

  diagnosand_names <- setdiff(names(diagnosands_df), group_by_set)

  # Calculate n_sims --------------------------------------------------------

  n_sims_df <- calculate_sims(simulations_df = simulations_df, group_by_set = group_by_set)

  # Bootstrap ---------------------------------------------------------------

  if (bootstrap_sims > 0) {
    bootout <- bootstrap_diagnosands(
      bootstrap_sims = bootstrap_sims,
      diagnosands = diagnosands,
      simulations_df = simulations_df,
      diagnosands_df = diagnosands_df,
      group_by_set = group_by_set
    )
    diagnosands_df <- bootout$diagnosands_df
  }

  # prep for return ---------------------------------------------------------

  diagnosands_df <- merge(x = diagnosands_df, y = n_sims_df, by = group_by_set, all = TRUE)

  parameters_df <- attr(simulations_df, "parameters")
  diagnosands_df <- merge_param_df(diagnosands_df, parameters_df)
  
  # Reorder rows
  sort_by_list <- c(group_by_set, "statistic") %icn% diagnosands_df
  diagnosands_df <- diagnosands_df[do.call(order, as.list(diagnosands_df[sort_by_list])), , drop = FALSE]
  
  rownames(diagnosands_df) <- NULL
  
  # Return frames
  out <- list(simulations_df = simulations_df, diagnosands_df = diagnosands_df, diagnosand_names = diagnosand_names, group_by_set = group_by_set, parameters_df = parameters_df)

  if (bootstrap_sims != 0) {
    out$bootstrap_replicates <- merge_param_df(bootout$diagnosand_replicates, parameters_df)
  }
  out$bootstrap_sims <- bootstrap_sims
  
  out$duration <- Sys.time() - start_time

  structure(out, class = "diagnosis")
}

#' @rdname diagnose_design
#' @export
diagnose_designs <- diagnose_design

reorder_columns <- function(a, b, n1 = colnames(a), n2 = colnames(b))
  c(n1, setdiff(n2, n1))

merge_param_df <- function(df, parameters_df) {

  if(!is.data.frame(parameters_df)) return(df)
  
  df <- merge(df, parameters_df, by = "design")
  
  # make design a factor
  df$design <- factor(df$design, levels = as.character(parameters_df$design))
  
  # reorder columns
  df <- df[, reorder_columns(parameters_df, df), drop = FALSE]
  
  df
}

check_design_class <- function(designs) {
  if (!all(sapply(designs, function(x) {
    inherits(x, "design") || (inherits(x, "function") && is.null(formals(x)))
  }))) {
    stop("Please only send design objects or functions with no arguments.")
  }
}

setup_diagnosands <- function(..., diagnosands) {
  designs <- dots_to_list_of_designs(...)

  diagnosands %||% lapply(designs, function(x) attr(x, "diagnosands") %||% default_diagnosands)
}

calculate_diagnosands <- function(simulations_df, diagnosands, group_by_set) {
  if ("design" %in% group_by_set) {
    group_by_list <- simulations_df[, "design", drop = FALSE]
    labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
    labels_df <- lapply(labels_df, head, n = 1)

    # ensure diagnosand functions are in the same order as designs
    # diagnosands <- diagnosands[names(labels_df)]

    if (is.list(diagnosands)) {
      diagnosands <- diagnosands[names(labels_df)]
    }

    simulations_list <- split(simulations_df, lapply(group_by_list, addNA), drop = TRUE)

    if (is.list(diagnosands)) {
      diagnosands_df <- mapply(calculate_diagnosands_single_design, simulations_list, diagnosands,
        MoreArgs = list(group_by_set), SIMPLIFY = FALSE
      )
    } else {
      diagnosands_df <- mapply(calculate_diagnosands_single_design, simulations_list,
        MoreArgs = list(
          diagnosands = diagnosands,
          group_by_set = group_by_set
        ),
        SIMPLIFY = FALSE
      )
    }

    diagnosands_df <- rbind_disjoint(diagnosands_df)
  } else {
    diagnosands_df <- calculate_diagnosands_single_design(
      simulations_df, diagnosands[[1]], group_by_set
    )
  }

  diagnosands_df
}

calculate_diagnosands_single_design <- function(simulations_df, diagnosands, group_by_set) {
  group_by_list <- simulations_df[, group_by_set, drop = FALSE]
  
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)

  diagnosands_df <- split(simulations_df, lapply(group_by_list, addNA), drop = TRUE)
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
  
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)

  n_sims_df <- split(simulations_df, lapply(group_by_list, addNA), drop = TRUE)
  n_sims_df <- lapply(n_sims_df, FUN = function(x) {
    data.frame(n_sims = nrow(x))
  })

  # c appropriate and fast here bc labels_df must be a list (drop=FALSE above)
  n_sims_df <- as.data.frame(t(mapply(c, labels_df, n_sims_df)), stringsAsFactors = FALSE)
  n_sims_df[] <- lapply(n_sims_df, unlist)

  n_sims_df
}


bootstrap_diagnosands <- function(bootstrap_sims, simulations_df, diagnosands, diagnosands_df, group_by_set) {
  bootstrap_level <- if ("step_1_draw" %in% names(simulations_df)) "step_1_draw" else "sim_ID"

  boot_indicies_by_id <- split(seq_len(nrow(simulations_df)), simulations_df[, bootstrap_level])
  nsims <- max(simulations_df[, bootstrap_level])

  boot_function <- function() {
    boot_ids <- sample.int(nsims, nsims, TRUE)
    boot_indicies <- unlist(boot_indicies_by_id[boot_ids])
    calculate_diagnosands(
      simulations_df = simulations_df[boot_indicies, , drop = FALSE],
      diagnosands = diagnosands,
      group_by_set = group_by_set
    )
  }

  # Make a list of diagnosand replicates
  diagnosand_replicates <- future_lapply(seq_len(bootstrap_sims),
    function(i) cbind(bootstrap_id = i, boot_function()), # Add bootstrap ID
    future.seed = NA,
    future.globals = "boot_function"
  )

  # Make a data.frame of diagnosand replicates
  diagnosand_replicates <- do.call(rbind.data.frame, diagnosand_replicates)
  rownames(diagnosand_replicates) <- NULL

  # Prep for se calculation
  group_by_list <- diagnosand_replicates[, group_by_set, drop = FALSE]
  labels_df <- split(group_by_list, lapply(group_by_list, addNA), drop = TRUE)
  labels_df <- lapply(labels_df, head, n = 1)

  diagnosands_df_group <- diagnosands_df[, group_by_set, drop = FALSE]
  diagnosands_df[group_by_set] <- NULL
  diagnosands_names <- colnames(diagnosands_df)

  diagnosands_df <- split(diagnosands_df, lapply(diagnosands_df_group, addNA), drop = TRUE)
  diagnosands_df <- diagnosands_df[names(labels_df)]

  # Calculate standard errors
  use_vars <- setdiff(names(diagnosand_replicates), c(group_by_set, "bootstrap_id"))
  diagnosands_se_df <- split(diagnosand_replicates[use_vars], lapply(group_by_list, addNA), drop = TRUE)
  diagnosands_se_df <- lapply(diagnosands_se_df, lapply, sd)

  # Clean up
  diagnosands_se_df <- lapply(diagnosands_se_df, setNames, sprintf("se(%s)", diagnosands_names))

  diagnosands_df <- as.data.frame(t(mapply(c, labels_df, diagnosands_df, diagnosands_se_df)),
    stringsAsFactors = FALSE
  )
  diagnosands_df[] <- lapply(diagnosands_df, unlist)

  # Permute columns so SEs are right of diagnosands
  n_diag <- length(diagnosands_names)
  i <- c(
    seq_along(group_by_set),
    length(group_by_set) + rep(seq_len(n_diag), each = 2) + c(0, n_diag)
  )
  diagnosands_df <- diagnosands_df[, i, drop = FALSE]

  # Reordering columns
  dim_cols <- c("estimator", "outcome", "term", "inquiry") %i% group_by_set
  ix <- sort(match(dim_cols, colnames(diagnosands_df)))
  diagnosands_df[ix] <- diagnosands_df[dim_cols]
  names(diagnosands_df)[ix] <- dim_cols

  list(
    diagnosands_df = diagnosands_df,
    diagnosand_replicates = diagnosand_replicates
  )
}

#' @rdname diagnose_design
#'
#' @importFrom rlang as_label
#' @export
vars <- function(...) {
  quos(...)
}
