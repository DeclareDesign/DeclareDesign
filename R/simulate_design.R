#' Simulate a design
#'
#' Runs many simulations of a design and returns a simulations data.frame.
#'
#' @param ... A design created using the + operator, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{expand_design}}.
#'
#' @param sims The number of simulations, defaulting to 500. If sims is a vector of the form c(10, 1, 2, 1) then different steps of a design will be simulated different numbers of times.
#'
#' @importFrom stats setNames
#' @importFrom rlang is_list is_bare_integerish
#' @importFrom utils head type.convert
#' @export
#' @examples
#' my_model <- 
#'   declare_model(
#'     N = 500, 
#'     U = rnorm(N),
#'     Y_Z_0 = U, 
#'     Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
#'   )
#'
#' my_assignment <- declare_assignment(Z = complete_ra(N), legacy = FALSE)
#'
#' my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
#'
#' my_reveal <- declare_measurement(Y = reveal_outcomes(Y ~ Z))
#'
#' design <- my_model +
#'   my_inquiry +
#'   my_assignment +
#'   my_reveal +
#'   my_estimator
#'
#' \dontrun{
#' simulations <- simulate_design(designs, sims = 2)
#' diagnosis <- diagnose_design(simulations_df = simulations)
#' }
#'
#' \dontrun{
#' # A fixed population with simulations over assignment only
#' head(simulate_design(design, sims = c(1, 1, 1, 100, 1)))
#' }
#'
#' @details
#'
#' Different steps of a design may each be simulated different a number of times, as specified by sims. In this case simulations are grouped into "fans". The nested 
#' structure of simulations is recorded in the dataset using a set of variables named "step_x_draw." For example if sims = c(2,1,1,3) is passed to simulate_design, then there
#' will be two distinct draws of step 1, indicated in variable "step_1_draw" (with values 1 and 2) and there will be three draws for step 4 within each of the step 1 draws, recorded in "step_4_draw" (with values 1 to 6).
#'    
simulate_design <- function(..., sims = 500) {
  designs <- dots_to_list_of_designs(...)
  
  # if you provide a list of sims for each design, i.e.
  #   sims = list(my_design_1 = c(100, 1, 1), my_design_2 = 200)
  # use it! otherwise, create a list of length designs that repeats the sims
  if (!is_list(sims)) {
    sims <- lapply(seq_along(designs), function(i) sims)
  }
  
  # Simulate One or More Designs
  simulations_list <- mapply(
    design = designs,
    sims = sims,
    FUN = simulate_single_design,
    SIMPLIFY = FALSE
  )
  
  simulations_list <- Map(cbind, design_label = names(simulations_list), simulations_list, stringsAsFactors = FALSE)
  
  # Cleanup
  simulations_df <- rbind_disjoint(simulations_list)
  rownames(simulations_df) <- NULL
  
  # Obtain all parameters
  
  parameters_df_list <- lapply(designs, function(x) attr(x, "parameters"))
  parameters_df_list <- lapply(seq_along(parameters_df_list), function(i) {
    out <- data.frame(design_label = names(parameters_df_list[i]))
    if (!is.null(parameters_df_list[[i]])) {
      out <- data.frame(out, parameters_df_list[[i]])
    }
    out
  })
  parameters_df <- rbind_disjoint(parameters_df_list)
  parameters_df <- data.frame(lapply(parameters_df, type_convert), stringsAsFactors = FALSE)
  
  simulations_df <- merge(simulations_df, parameters_df, by = "design_label", sort = FALSE, all = TRUE)
  
  simulations_df <- simulations_df[, reorder_columns(parameters_df, simulations_df), drop = FALSE]
  
  attr(simulations_df, "parameters") <- parameters_df
  
  simulations_df
}

#' @rdname simulate_design
#' @export
simulate_designs <- simulate_design



#' @importFrom rlang as_list
simulate_single_design <- function(design, sims, low_simulations_warning = TRUE) {
  if (!is_bare_integerish(sims) || (length(design) != length(sims) & length(sims) != 1)) {
    stop("Please provide sims a scalar or a numeric vector of length the number of steps in designs.", call. = FALSE)
  }
  
  if (min(sims) < 1) {
    stop("Sims should be >= 1", call. = FALSE)
  }
  
  # See also ?testthat::is_testing
  if (low_simulations_warning && sims[1] < 30 && !identical(Sys.getenv("TESTTHAT"), "true")) {
    warning(
      "We recommend you choose a higher number of simulations than ",
      sims[1],
      " for the top level of simulation.",
      call. = FALSE
    )
  }
  
  # escape hatch for all ones
  if(prod(sims) == 1) sims <- 1
  
  # If sims is set correctly, fan out
  
  if (length(sims) == 1 && is.null(names(sims))) {
    results_list <- future_lapply(seq_len(sims),
                                  function(i)
                                    run_design_internal(design),
                                  future.seed = NA, future.globals = "design"
    )
  } else {
    sims <- check_sims(design, sims)
    results_list <- fan_out(design, sims)
    
    
    
  }
  
  results2x <- function(results_list, what) {
    subresult <- lapply(results_list, `[[`, what)
    # df <- do.call(rbind.data.frame, subresult)
    df <- rbind_disjoint(subresult)
    if (nrow(df) == 0) {
      return(df)
    }
    
    df <- cbind(sim_ID = rep(seq_along(subresult), vapply(subresult, nrow, 0L)), df)
  }
  
  estimates_df <- results2x(results_list, "estimates_df")
  inquiries_df <- results2x(results_list, "inquiries_df")
  
  if (is_empty(estimates_df) && is_empty(inquiries_df)) {
    stop("No estimates or inquiries were declared, so design cannot be simulated.", call. = FALSE)
  } else if (is_empty(inquiries_df)) {
    simulations_df <- estimates_df
  } else if (is_empty(estimates_df)) {
    simulations_df <- inquiries_df
  } else if (all(inquiries_df$inquiry_label %in% estimates_df$inquiry_label) &
             all(estimates_df$inquiry_label %in% inquiries_df$inquiry_label)){
    
    
    inquiries_df_split <- split(x = inquiries_df, f = inquiries_df$inquiry_label)
    estimates_df_split <- split(x = estimates_df, f = estimates_df$inquiry_label)
    
    non_missing_columns <- function(dat){
      nonmissing <- apply(dat, 2, FUN = function(x) any(!is.na(x)))
      return(dat[,nonmissing,drop = FALSE])
    }
    
    
    inquiries_df_split <- lapply(inquiries_df_split, non_missing_columns)
    estimates_df_split <- lapply(estimates_df_split, non_missing_columns)
    
    by_split <- lapply(X = seq_along(inquiries_df_split), 
                       FUN = function(x){
                         colnames(inquiries_df_split[[x]]) %icn% estimates_df_split[[x]]
                       })
    
    simulations_df_split <- 
      mapply(FUN = merge,
             x = inquiries_df_split,
             y = estimates_df_split,
             by = by_split,
             MoreArgs = list(all = TRUE, sort = FALSE),
             SIMPLIFY = FALSE
      )
    
    simulations_df <- rbind_disjoint(simulations_df_split)
  } else {
    
    simulations_df <- merge(
      inquiries_df,
      estimates_df,
      by = colnames(inquiries_df) %icn% estimates_df,
      all = TRUE,
      sort = FALSE
    )
    
    if (nrow(simulations_df) > max(nrow(inquiries_df), nrow(estimates_df))) {
      warning(
        "Estimators lack inquiry/term labels for matching, a many-to-many merge was performed."
      )
    }
  }
  
  # removed for now
  # if (!is_empty(attr(design, "parameters"))) {
  #   simulations_df <-
  #     data.frame(
  #       simulations_df[, 1, drop = FALSE],
  #       as_list(attr(design, "parameters")),
  #       simulations_df[, -1, drop = FALSE], 
  #       stringsAsFactors = FALSE
  #     )
  # }
  # simulations_df <- data.frame(lapply(simulations_df, type_convert), stringsAsFactors = FALSE)
  simulations_df
}


#' @importFrom rlang quo_squash is_call
infer_names <- function(x, type = "design") {
  inferred_names <- names(x) %||% rep("", length(x))
  
  inferred_names <- vapply(seq_along(x),
                           FUN.VALUE = "",
                           function(i, xi = x[[i]], nm = inferred_names[i]) {
                             if (nm != "") {
                               nm
                             } else if (!is_quosure(xi) || is_call(quo_squash(xi))) {
                               paste0(type, "_", i)
                             } else {
                               quo_text(xi)
                             }
                           }
  )
  
  # confirm no dupes
  is_dupes <- duplicated(inferred_names)
  if (any(is_dupes)) {
    stop("You have more than one ", type, " named ", inferred_names[is_dupes])
  }
  
  inferred_names
}

# TODO: remove this when 3.6 comes out
# for version 3.4 compatibility
#' @importFrom utils compareVersion
type_convert <- function(x) {
  if(compareVersion("3.5", paste(R.Version()$major, R.Version()$minor, sep = ".")) == -1){
    if (inherits(x, "character") || inherits(x, "factor")) type.convert(x, as.is = TRUE) else x
  } else {
    if (inherits(x, "character")) type.convert(x, as.is = TRUE) else x
  }
}
