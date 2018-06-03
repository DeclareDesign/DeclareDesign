#' Simulate a design
#'
#' Runs many simulations of a design and saves to a dataframe.
#'
#'
#' @param ... A design created by \code{\link{declare_design}}, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{expand_design}}.
#'
#' @param sims The number of simulations, defaulting to 500. If sims is a vector of the form c(10, 1, 2, 1) then different steps of a design will be simulated different numbers of times.  See details.
#'
#' @importFrom stats setNames
#' @importFrom rlang is_list
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
#' }
#'
#' @details
#'
#' Different steps of a design may each be simulated different a number of times, as specified by sims. In this case simulations are grouped into "fans", eg "fan_1" indicates all the simulations that have the same draw from the first level of the design. For efficiency there are generally fewer fans than design steps where all contiguous steps with 1 sim specified are combined into a single fan.
simulate_design <-
  function(..., sims = 500) {
    designs_quos <- quos(...)
    designs <- lapply(designs_quos, eval_tidy)
    
    ## Two cases:
    ## 1. send one or more design objects created by declare_design
    ## 2. send a single list of design objects e.g. created by expand_design
    ## Approach: unpack designs if a list of designs was sent as a single list object
    if (length(designs) == 1 &&
        is.list(designs[[1]]) &&
        !"design" %in% class(designs[[1]])) {
      designs <- designs[[1]]
      names(designs) <- infer_names_list(designs)
    } else {
      names(designs) <- infer_names_quos(designs_quos)
    }
    
    ## Do not allow users to send more than one object if any is not a design object
    if (!all(vapply(designs, inherits, FALSE, "design"))) {
      stop("Please only send design objects to simulate_design.")
    }
    
    # if you provide a list of sims for each design, i.e.
    #   sims = list(my_design_1 = c(100, 1, 1), my_design_2 = 200)
    # use it! otherwise, create a list of length designs that repeats the sims
    if (!is_list(sims)) {
      sims <- lapply(1:length(designs), function(i)
        sims)
    }
    
    sims_match_steps <-
      sapply(designs, length) == sapply(sims, length) |
      sapply(sims, length) == 1
    if (!all(sims_match_steps)) {
      wrong_designs <- names(designs)[which(!sims_match_steps)]
      stop(
        "The sims argument you provided for the designs named ",
        paste(wrong_designs, collapse = ", "),
        "are not correct. Sims should be of length the number of steps in a design or one."
      )
    }
    
    # Simulate One or More Designs
    simulations_list <- mapply(
      design = designs,
      sims = sims,
      FUN = simulate_single_design,
      SIMPLIFY = FALSE
    )
    
    if (length(designs) > 1) {
      simulations_list <-
        Map(cbind, design_ID = names(simulations_list), simulations_list, stringsAsFactors = FALSE)
    }
    
    # Cleanup
    simulations_df <- rbind_disjoint(simulations_list)
    rownames(simulations_df) <- NULL
    
    # # Check that there are the expected number of simulations
    # check_sim_number(simulations_df, sims)
    
    simulations_df
    
  }

#' Run Single Design -- Basic simulation script that is called on by simulate_design

#' @importFrom rlang as_list
simulate_single_design <-
  function(design, sims) {
    if (min(sims) < 1)
      stop("Sims should be >= 1")
    
    ### If sims is set correctly, fan out
    
    if (length(sims) == 1 && is.null(names(sims))) {
      results_list <- future_lapply(seq_len(sims),
                                    function(i)
                                      run_design(design),
                                    future.seed = NA, future.globals = "design")
    } else {
      sims <- check_sims(design, sims)
      results_list <- fan_out(design, sims)
      fan_id <- setNames(rev(do.call(expand.grid, lapply(
        rev(sims$n), seq
      ))), paste0("fan_", seq_len(nrow(sims))))
      fan_id$sim_ID <- seq_len(nrow(fan_id))
    }
    
    results2x <- function(results_list, what) {
      subresult <- lapply(results_list, `[[`, what)
      df <- do.call(rbind.data.frame, subresult)
      if (nrow(df) == 0)
        return(df)
      
      df <-
        cbind(sim_ID = rep(seq_along(subresult), vapply(subresult, nrow, 0L)), df)
    }
    
    estimates_df <- results2x(results_list, "estimates_df")
    estimands_df <- results2x(results_list, "estimands_df")
    
    if (nrow(estimates_df) == 0 & nrow(estimands_df) == 0) {
      stop("No estimates or estimands were declared, so design cannot be simulated.", call. = FALSE)
    } else if (nrow(estimands_df) == 0 && nrow(estimates_df) > 0) {
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
        warning(
          "Estimators lack estimand/coefficient labels for matching, a many-to-many merge was performed."
        )
      }
    }
    
    if (exists("fan_id")) {
      simulations_df <- merge(simulations_df, fan_id, by = "sim_ID")
    }
    
    if (!is_empty(attr(design, "parameters"))) {
      simulations_df <-
        data.frame(simulations_df[, 1, drop = FALSE],
                   as_list(attr(design, "parameters")),
                   simulations_df[, -1, drop = FALSE])
    }
    
    simulations_df
    
  }


infer_names_list <-
  function(x, type = "design") {
    
    if (!is.null(names(x))) {
      inferred_names <- names(x)
    } else {
      inferred_names <- rep("", length(x))
    }
    missing_names <- inferred_names == ""
    
    if (any(missing_names)) {
      inferred_names[missing_names] <-
        paste0(type, "_", which(missing_names))
    }
    
    # confirm no dupes
    if (any(duplicated(inferred_names))) {
      stop("You have more than one design named ", inferred_names[duplicated(inferred_names)])
    }
    
    inferred_names
  }

#' @importFrom rlang quo_squash is_call
infer_names_quos <-
  function(x, type = "design") {
    if (!is.null(names(x))) {
      inferred_names <- names(x)
    } else {
      inferred_names <- rep("", length(x))
    }
    missing_names <- inferred_names == ""
    x_is_call <- sapply(sapply(x, quo_squash, simplify = FALSE), is_call)
    
    if (any(missing_names)) {
      inferred_names[missing_names & x_is_call] <-
        paste0(type, "_", which(missing_names & x_is_call))
      inferred_names[missing_names & !x_is_call] <-
        sapply(x, quo_text)[missing_names & !x_is_call]
    }
    
    # confirm no dupes
    if (any(duplicated(inferred_names))) {
      stop("You have more than one design named ", inferred_names[duplicated(inferred_names)])
    }
    
    inferred_names
    
    
  }
