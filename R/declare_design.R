

#' Declare Design
#'
#' @param ... A set of steps in a research design, beginning with a \code{data.frame} representing the population or a function that draws the population. Steps are evaluated sequentially. With the exception of the first step, all steps must be functions that take a \code{data.frame} as an argument and return a \code{data.frame}. Typically, many steps are declared using the \code{declare_} functions, i.e., \code{\link{declare_population}}, \code{\link{declare_population}}, \code{\link{declare_sampling}}, \code{\link{declare_potential_outcomes}}, \code{\link{declare_estimand}}, \code{\link{declare_assignment}}, and \code{\link{declare_estimator}}. Functions from the \code{dplyr} package such as mutate can also be usefully included.
#' @param title (optional) The title of the study, as a character string.
#' @param authors (optional) The authors of the study, as a character string.
#' @param description (optional) A description of the design in words, as a character string, stored alongside the declaration in code.
#' @param citation (optional) The preferred citation for the design, as a character string. Either include the full citation in text, or paste a BibTeX entry. If title and authors are specified and you leave citation empty, a BibTeX entry will be created automatically.
#'
#' @details
#'
#' Users can supply three kinds of functions to declare_design:
#'
#' 1. Data generating functions. These include population, assignment, and sampling functions.
#'
#' 2. Estimand functions.
#'
#' 3. Estimator functions.
#'
#' The location of the estimand and estimator functions in the chain of functions determine *when* the values of the estimand and estimator are calculated. This allows users to, for example, differentiate between a population average treatment effect and a sample average treatment effect by placing the estimand function before or after sampling.
#'
#' Designs declared with declare_design can be investigated with a series of post-declaration commands, such as \code{\link{draw_data}}, \code{\link{get_estimands}}, \code{\link{get_estimates}}, and \code{\link{diagnose_design}}.
#'
#' The print and summary methods for a design object return some helpful descriptions of the steps in your research design. If randomizr functions are used for any assignment or sampling steps, additional details about those steps are provided.
#'
#' @return a list of two functions, the \code{design_function} and the \code{data_function}. The \code{design_function} runs the design once, i.e. draws the data and calculates any estimates and estimands defined in \code{...}, returned separately as two \code{data.frame}'s. The \code{data_function} runs the design once also, but only returns the final data.
#'
#' @importFrom rlang quos quo_expr eval_tidy quo_text lang_args is_formula
#' @importFrom utils bibentry
#' @export
#'
#' @examples
#'
#' my_population <- declare_population(N = 500, noise = rnorm(N))
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
#' design
#'
#' df <- draw_data(design)
#'
#' estimates <- get_estimates(design)
#' estimands <- get_estimands(design)
#'
#' \dontrun{
#' diagnosis <- diagnose_design(design)
#'
#' summary(diagnosis)
#' }
#'
declare_design <- function(...,
                           title = NULL,
                           authors = NULL,
                           description = NULL,
                           citation = NULL) {
  # process bibtex

  timestamp <- Sys.time()

  if ((is.null(citation) | class(citation) == "character") &
      !is.null(title) & !is.null(authors)) {
    citation <- bibentry(
      "unpublished",
      title = title,
      author = authors,
      note = "Unpublished research design declaration.",
      month = format(timestamp, "%b"),
      year = format(timestamp, "%Y"),
      textVersion = citation
    )
  }

  # Some preprocessing

  causal_order_env <- freeze_environment(parent.frame())

  dots <- quos(...)

  causal_order <- list()
  causal_order_expr <- list()
  for(i in seq_along(dots))
    causal_order_expr[[i]] <- quo_expr(dots[[i]])

  name_or_call <- sapply(causal_order_expr, class)

  ## wrap any call in wrap_step()
  # if (length(dots) > 1) {
  #   for (i in 2:length(dots)) {
  #     if (name_or_call[[i]] == "call") {
  #       dots[[i]] <-
  #         quo(wrap_step(!!dots[[i]]))  ##call("wrap_step", quo_expr(dots[[i]]))
  #     }
  #   }
  # }

  for(i in seq_along(dots)) {
    causal_order[[i]] <- tryCatch(
       eval_tidy(dots[[i]]),
      error = function(e) eval_tidy(wrap_step(!!dots[[i]]))
    )
  }

  # Special case for initializing with a data.frame
  if("data.frame" %in% class(causal_order[[1]])){
    causal_order[[1]] <- local({
      df <- causal_order[[1]]
      structure(function(data) df, type='seed.data')
    })
  }


  function_types <- vapply(causal_order, attr, NA_character_, "type")

  causal_order_types <- function_types
  causal_order_types[!function_types %in% c("estimand", "estimator")] <- "dgp"

  local({

    labels <- sapply(causal_order, attr, "label")

    check_unique_labels <- function(labels, types, what){
      ss <- labels[types == what]
      if(anyDuplicated(ss)) stop(
        "You have ", what, "s with identical labels: ",
        unique(ss[duplicated(ss)]),
        "\nPlease provide ", what, "s with unique labels"

        )

    }

    check_unique_labels(labels, function_types, "estimand")
    check_unique_labels(labels, function_types, "estimator")

  })

  # this extracts the "DGP" parts of the causal order and runs them.
  data_function <- function() {
    current_df <- NULL

    for(f in causal_order[causal_order_types == "dgp"])
      current_df <- f(current_df)

    current_df
  }

  # This does causal order step by step; saving calculated estimands and estimates along the way

  design_function <- function() {
    current_df <- NULL

    results <- list(estimand=vector("list", length(causal_order)),
                    estimator=vector("list", length(causal_order)))

    for (i in seq_along(causal_order)) {
      function_type <- causal_order_types[[i]]
      df <- causal_order[[i]](current_df)

      # if it's a dgp
      if (function_type == "dgp") {
        current_df <- df
      } else {
        results[[function_type]][[i]] <- df
      }
    }
    list(estimates_df = do.call(rbind.data.frame, results[["estimator"]]),
         estimands_df = do.call(rbind.data.frame, results[["estimand"]]))
  }


  return(structure(
    list(
      data_function = data_function,
      design_function = design_function,
      causal_order_expr = causal_order_expr,
      causal_order_env = causal_order_env,
      function_types = function_types,
      causal_order_types = causal_order_types,
      causal_order = causal_order,
      title = title,
      authors = authors,
      description = description,
      citation = citation,
      timestamp = timestamp,
      call = match.call()
    ),
    class = "design"
  ))

}

#TODO move to utils
get_added_variables <- function(last_df = NULL, current_df) {
  setdiff(names(current_df), names(last_df))
}

get_modified_variables <- function(last_df = NULL, current_df) {
  is_modified <- function(j) !isTRUE(all.equal(last_df[[j]], current_df[[j]]))
  shared <- intersect(names(current_df), names(last_df))

  Filter(is_modified, shared)
}



summary_function <- function(causal_order, causal_order_types) {
  get_formula_from_step <- function(step){
    call <- attributes(step)$call
    type <- attributes(step)$type
    if (!is.null(call) & !is.null(type) & type != "declare_step") {
      args <- lang_args(call)
      has_formula <- sapply(args, is_formula)
      formulae <- args[has_formula]
      if (length(formulae) == 1) {
        return(formulae[[1]])
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }


  variables_added <- variables_modified <-
    quantities_added <- quantities_modified <-
    N <-
    vector("list", length(causal_order))

  formulae <- lapply(causal_order, get_formula_from_step)

  current_df <- process_population(causal_order[[1]])

  var_desc <- variables_added[[1]] <- lapply(current_df, describe_variable)

  N[[1]] <- paste0("N = ", nrow(current_df))

  estimates_df <- estimands_df <- data.frame()

  last_df <- current_df

  if (length(causal_order) > 1) {
    for (i in 2:length(causal_order)) {
      # if it's a dgp
      if (causal_order_types[i] == "dgp") {
        current_df <- causal_order[[i]](last_df)

        variables_added_names <-
          get_added_variables(last_df = last_df, current_df = current_df)

        variables_modified_names <-
          get_modified_variables(last_df = last_df, current_df = current_df)

        if (!is.null(variables_added_names)) {
          variables_added[[i]] <-
            lapply(current_df[, variables_added_names, drop = FALSE],
                   describe_variable)
          var_desc[variables_added_names] <- variables_added[[i]]
        }

        if (!is.null(variables_modified_names)) {
          v_mod <- lapply(current_df[, variables_modified_names, drop=FALSE],
                          describe_variable)
          variables_modified[[i]] <- mapply(list,
                                            before=var_desc[variables_modified_names],
                                            after=v_mod,
                                            SIMPLIFY = FALSE, USE.NAMES = TRUE)
          var_desc[variables_modified_names] <- v_mod

        }

        # NJF 10/25 Dead Feature???
        # if (!is.null(attributes(causal_order[[i]])$summary_function)) {
        #   quantities_added[[i]] <-
        #     capture.output(attributes(causal_order[[i]])$summary_function(last_df))
        # }

        N[i] <- local({
          c_row <- nrow(current_df)
          l_row <- nrow(last_df)
          if(c_row == l_row) list(NULL) else
            sprintf("N = %d (%d %s)", c_row, abs(c_row - l_row),
                    ifelse(c_row > l_row, "added", "subtracted"))
        })

        last_df <- current_df

      } else if (causal_order_types[i] %in% c("estimand", "estimator")) {
        quantities_added[[i]] <- causal_order[[i]](current_df)
      }
    }
  }
  structure(
    list(variables_added = variables_added,
         quantities_added = quantities_added,
         variables_modified = variables_modified,
         N = N,
         formulae = formulae),
    class = "design_summary"
  )
}


process_population <- function(population) {
  ## the first part of the DGP must be a data.frame. Take what the user creates and turn it into a data.frame.
  if ("data.frame" %in% class(population)) {
    current_df <- population
  # } else if (class(population) == "call") {
  #   tryCatch(current_df <- population, error=function(e)stop("The first element of your design must be a data.frame or a function that returns a data.frame. The population call provided failed:", e))
  #   if (!"data.frame" %in% class(current_df)) {
  #     stop(
  #       "The first element of your design must be a data.frame or a function that returns a data.frame. You provided a called that did not return a data.frame."
  #     )
  #   }
  } else if (class(population) == "function") {
    tryCatch(current_df <- population(), error=function(e)stop("The first element of your design must be a data.frame or a function that returns a data.frame. The population function provided failed:", e))
    if (!exists("current_df") |
        !any(class(current_df) == "data.frame")) {
      stop(
        "The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame."
      )
    }
  } else {
    stop(
      "The first element of your design must be a data.frame or a function that returns a data.frame."
    )
  }
  return(current_df)
}

