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
#' design <- my_population +
#'   my_potential_outcomes +
#'   my_estimand +
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
#' Different steps of a design may each be simulated different a number of times, as specified by sims. In this case simulations are grouped into "fans", eg "fan_1" indicates all the simulations that have the same draw from the first level of the design. For efficiency there are generally fewer fans than design steps where all contiguous steps with 1 sim specified are combined into a single fan.
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

  simulations_df <- simulations_df[, reorder_columns(parameters_df, simulations_df), drop = FALSE]

  attr(simulations_df, "parameters") <- parameters_df

  simulations_df
}


#' @importFrom rlang as_list
simulate_single_design <- function(design, sims) {
  if (!is_bare_integerish(sims) || (length(design) != length(sims) & length(sims) != 1)) {
    stop("Please provide sims a scalar or a numeric vector of length the number of steps in designs.", call. = FALSE)
  }

  if (min(sims) < 1) {
    stop("Sims should be >= 1", call. = FALSE)
  }

  if (length(sims) > 1 && sims[1] < 30) {
    warning(
      "We recommend you choose a higher number of simulations than ",
      sims[1],
      " for the top level of simulation.",
      call. = FALSE
    )
  }

  # If sims is set correctly, fan out

  if (length(sims) == 1 && is.null(names(sims))) {
    results_list <- future_lapply(seq_len(sims),
      function(i)
        run_design(design),
      future.seed = NA, future.globals = "design"
    )
  } else {
    sims <- check_sims(design, sims)
    results_list <- fan_out(design, sims)
    fan_id <- setNames(
      lapply(rev(sims$n), seq),
      paste0("fan_", seq_len(nrow(sims)))
    )
    fan_id <- expand.grid(fan_id)
    fan_id$sim_ID <- seq_len(nrow(fan_id))
  }

  results2x <- function(results_list, what) {
    subresult <- lapply(results_list, `[[`, what)
    df <- do.call(rbind.data.frame, subresult)
    if (nrow(df) == 0) {
      return(df)
    }

    df <- cbind(sim_ID = rep(seq_along(subresult), vapply(subresult, nrow, 0L)), df)
  }

  estimates_df <- results2x(results_list, "estimates_df")
  estimands_df <- results2x(results_list, "estimands_df")

  if (is_empty(estimates_df) && is_empty(estimands_df)) {
    stop("No estimates or estimands were declared, so design cannot be simulated.", call. = FALSE)
  } else if (is_empty(estimands_df)) {
    simulations_df <- estimates_df
  } else if (is_empty(estimates_df)) {
    simulations_df <- estimands_df
  } else {
    simulations_df <- merge(
      estimands_df,
      estimates_df,
      by = colnames(estimands_df) %icn% estimates_df,
      all = TRUE,
      sort = FALSE
    )

    if (nrow(simulations_df) > max(nrow(estimands_df), nrow(estimates_df))) {
      warning(
        "Estimators lack estimand/term labels for matching, a many-to-many merge was performed."
      )
    }
  }

  if (exists("fan_id")) {
    simulations_df <- merge(simulations_df, fan_id, by = "sim_ID")
  }

  if (!is_empty(attr(design, "parameters"))) {
    simulations_df <-
      data.frame(
        simulations_df[, 1, drop = FALSE],
        as_list(attr(design, "parameters")),
        simulations_df[, -1, drop = FALSE]
      )
  }

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

type_convert <- function(x) {
  if (inherits(x, "character")) type.convert(x, as.is = TRUE) else x
}
