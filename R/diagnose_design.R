#' Run a design many times
#'
#' Runs many simulations of a design and saves to a dataframe.
#'
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{expand_design}}.
#'
#' @param sims The number of simulations, defaulting to 500. If sims is a vector of the form c(10, 1, 2, 1) then different steps of a design will be simulated different numbers of times.  See details.
#'
#' @importFrom stats setNames
#' @importFrom utils head
#' @export
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
#' simulations <- simulate_design(designs, sims = 2)
#' diagnosis   <- diagnose_design(simulations_df = simulations)
#' }
#'
#' \dontrun{
#' # A fixed population with simulations over assignment only
#' head(simulate_design(design, sims = c(1,1,1,100,1)))
#'}
#'
#'@details
#'
#' Different steps of a design may each be simulated different a number of times, as specified by sims. In this case simulations are grouped into "fans", eg "fan_1" indicates all the simulations that have the same draw from the first level of the design. For efficiency there are generally fewer fans than design steps where all contiguous steps with 1 sim specified are combined into a single fan.

simulate_design <- function(...,  sims = 500, add_parameters = FALSE) {

  designs <- list(...)

  inferred_names <- paste(substitute(list(...)))[-1]

  ## three cases:
  ## 1. send one or more design objects created by declare_design
  ## 2. send a single list of design objects created by expand_design
  ## 3. do not allow sending more than one object if any of them aren't design objects.
  if (length(designs) == 1 && is.list(designs[[1]]) && !"design" %in% class(designs[[1]]) ) {
    ## this unpacks designs if a list of designs was sent as a single list object, i.e.
    ##   as created by expand_design
    designs <- designs[[1]]
    if (!is.null(names(designs))) {
      inferred_names <- names(designs)
    } else {
      inferred_names <- paste0("design_", 1:length(designs))
    }
  }

  if (!all(vapply(designs, inherits, FALSE, "design"))) {
    stop("Please only send design objects to simulate_design.")
  }

  if (is.null(names(designs))) {
    names(designs) <- inferred_names
  } else {
    names(designs)[names(designs) == ""] <- inferred_names[names(designs) == ""]
  }

  if(length(designs) == 1) {
    out <- simulate_single_design(designs[[1]], sims = sims, add_parameters = add_parameters)
    return(out)
  }

  # comparison_sims
  simulations_list <- lapply(designs,
                            simulate_single_design,
                            sims = sims,
                            add_parameters = add_parameters
  )

#  for (i in 1:length(simulations_list)) {
#    simulations_list[[i]] <- cbind(design_ID = names(simulations_list)[i], simulations_list[[i]])
#  }

#   simulations_list <- lapply(comparison_sims, function(x) x$simulations)

  simulations_list <- Map(cbind,  design_ID = names(simulations_list), simulations_list )

  # Create an empty column for each non-existing attribute
  col_list <- unique(unlist(as.vector(sapply(simulations_list, names))))
  simulations_list <- lapply(simulations_list, function(x) {
    if(!identical(col_list , colnames(x))){
      missing_cols <- col_list[!col_list %in% colnames(x)]
      append  <- replicate(length(missing_cols), rep(NA, sims))
      colnames(append) <- missing_cols
      x <- cbind(x, append)}
    x <- x[,col_list]
  })




  simulations_df <- do.call(rbind, simulations_list)

  simulations_df <- data.frame(simulations_df)

  rownames(simulations_df) <- NULL

  # Block to reorder  columns if add_parameters is TRUE
  if(add_parameters){
    allcolnames <- colnames(simulations_df)
    Front <- c("design_ID", "sim_ID")
    Front <- Front[Front %in%  allcolnames]
    Parnames <- sort({
      unique(unlist(lapply(designs, function(j) {if(!is_empty(attr(j , "parameters")))
        names(attr(j , "parameters"))})))})
    End <- allcolnames[!(allcolnames %in% c(Front, Parnames))]
    simulations_df <- simulations_df[, c(Front, Parnames, End)]
    }

  attr(simulations_df, "sims") <- sims

  structure(simulations_df)


}



####################

#' Run Single Design -- Basic simulation script that is called on by simulate_design

simulate_single_design <- function(design, sims, add_parameters = FALSE) {

### If sims is set correctly, fan out

if(length(sims) == 1 && is.null(names(sims))) {
  results_list <- future_lapply(seq_len(sims),
                                function(i) run_design(design),
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

# Optionally add columns containing parameter attributes of a design

if(add_parameters){ if(!is_empty(attr(design , "parameters"))){
  parameters <- sapply(attr(design , "parameters"), rep, sims)
  if(sims == 1) parameters <- t(parameters)
  simulations_df <-
  cbind(parameters, simulations_df)
}}

attr(simulations_df, "sims") <- sims

structure(simulations_df)

}




#' Diagnose the Design
#'
#' Applies a diagnosand function to the simulations of a design results.
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{expand_design}}.
#' @param simulations A dataframe with simulations of a design. user must provide either a simulations data frame or a design or list of designs. Should have a sims attribute indicating the number of simulations used.
#' @param diagnosands A set of diagnosands created by \code{\link{declare_diagnosands}}. By default, these include bias, root mean-squared error, power, frequentist coverage, the mean and standard deviation of the estimate(s), the "type S" error rate (Gelman and Carlin 2014), and the mean of the estimand(s).
#' @param grouping_variables A set of variables used to generate groups of simulations for diagnosis. Defaults to c("design_ID", "estimand_label", "estimator_label", "coefficient")
#'
#' @param sims The number of simulations, defaulting to 500. sims may also be a vector indicating the number of simulations for each step in a design, as described for \code{\link{simulate_design}}
#' @param bootstrap Number  of bootstrap replicates for the diagnosands to obtain the standard errors of the diagnosands, defaulting to \code{100}.
#'
#' @details
#'
#' If the diagnosand function contains a \code{group_by} attribute, it will be used to split-apply-combine diagnosands rather than the intersecting column names.
#'
#' If \code{sims} is named, or longer than one element, a fan-out strategy is created and used instead.
#'
#' If the \code{future} package is installed, you can set  \code{\link[future]{plan}} to run multiple simulations at once.
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
#' # Using an existing data frame of simulations
#' \dontrun{
#' simulations <- simulate_design(designs, sims = 2)
#' diagnosis   <- diagnose_design(simulations_df = simulations)
#'
#' }

#' @importFrom stats setNames
#' @importFrom utils head
#' @export
diagnose_design <- function(..., simulations_df = NULL, add_parameters = FALSE,
                            diagnosands = default_diagnosands,
                            sims = 500, bootstrap = 100,
                            grouping_variables = c("design_ID", "estimand_label", "estimator_label", "coefficient")
                            ) {
  designs <- list(...)


  diagnosands

  ## three cases:
  ## 1. send one or more design objects created by declare_design
  ## 2. send a single list of design objects created by expand_design
  ## 3. do not allow sending more than one object if any of them aren't design objects.
  if (length(designs) == 1 && is.list(designs[[1]]) && !"design" %in% class(designs[[1]]) ) {
    ## this unpacks designs if a list of designs was sent as a single list object, i.e.
    ##   as created by expand_design
    designs <- designs[[1]]
    if (!is.null(names(designs))) {
      inferred_names <- names(designs)
    } else {
      inferred_names <- paste0("design_", 1:length(designs))
    }
  }


  if(is.null(simulations_df) + (length(designs)==0) !=1) {
    stop("Must provide either a design or a simulations data frame (not both, not neither!)")}

  if(length(designs)>0){
    if (!all(vapply(designs, inherits, FALSE, "design"))) {
      stop("Please only send design objects to diagnose_design.")
    }}

  # Update sims to reflect actual simulation dataframe
  if(!is.null(simulations_df)) sims <- attr(simulations_df, "sims")

  if(is.null(simulations_df))  simulations_df <- simulate_design(designs, sims = sims, add_parameters = add_parameters)

  group_by_set <- attr(diagnosands, "group_by") %||%
    colnames(simulations_df) %i% grouping_variables

  ## Get the list of all parameter attributes in design list since these are to be preserved in dataframe
  if(add_parameters){
    Parnames <- sort({unique(unlist(
        lapply(designs, function(j) {
          if(!is_empty(attr(j , "parameters")))
          names(attr(j , "parameters"))
          }
        )))})
    group_by_set <- c(group_by_set, Parnames)}


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
  rnames <- rownames(diagnosands_df)

  ################### Bootstrapping standard errors
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


  # Reordering columns
    dim_cols <- c("estimator_label", "coefficient", "estimand_label") %i% group_by_set
    ix <- sort(match(dim_cols, colnames(diagnosands_df)))
    diagnosands_df[ix] <- diagnosands_df[dim_cols]
    names(diagnosands_df)[ix] <- dim_cols
  }

  # Reorder by estimator labels in design
  estimator_labels <- unique(simulations_df$estimator_label)
  if (length(estimator_labels) > 1) {
    estimator_f <- factor(diagnosands_df$estimator_label, estimator_labels)
    diagnosands_df <- diagnosands_df[order(estimator_f), , drop=FALSE]
  }

#  rownames(diagnosands_df) <- NULL
  rownames(diagnosands_df)  <- rnames
  attr(diagnosands_df, "sims")      <- sims
  attr(diagnosands_df, "bootstrap") <- bootstrap
  attr(diagnosands_df, "n_diagosands")   <-  nrow(diagnosands(simulations_df))


  structure(
      list(simulations = simulations_df, diagnosands = diagnosands_df),
      class = "diagnosis"
    )

  }


