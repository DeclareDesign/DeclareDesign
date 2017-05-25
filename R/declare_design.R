
#' Declare Design
#'
#' @param ... A set of steps in a research design, beginning with a \code{data.frame} representing the population or a function that draws the population. Steps are evaluated sequentially. With the exception of the first step, all steps must be functions that take a \code{data.frame} as an argument and return a \code{data.frame}. Typically, many steps are declared using the \code{declare_} functions, i.e., \code{\link{declare_population}}, \code{\link{declare_population}}, \code{\link{declare_sampling}}, \code{\link{declare_potential_outcomes}}, \code{\link{declare_estimand}}, \code{\link{declare_assignment}}, and \code{\link{declare_estimator}}. Functions from the \code{dplyr} package such as \code{\link{mutate}} can also be usefully included.
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
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom dplyr bind_rows
#' @importFrom fabricatr describe_variable
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
#' diagnosis <- diagnose_design(design)
#'
#' summary(diagnosis)
#'
declare_design <- function(...) {

  # Some preprocessing

  causal_order_env <- freeze_environment(parent.frame())

  dots <- lazy_dots(...)

  dots_classes <- sapply(dots, function(x) class(x$expr))

  ## wrap any call in wrap_step_()
  if (length(dots) > 1) {
    for (i in 2:length(dots)) {
      if (dots_classes[[i]] == "call") {
        dots[[i]]$expr <- call("wrap_step_", dots[[i]]$expr)
      }
    }
  }

  causal_order <- lazy_eval(dots)

  causal_order_text <- eval(substitute(alist(...)))

  name_or_call <- sapply(causal_order_text, class)

  function_types <- rep("", length(causal_order))

  function_types[name_or_call == "name"] <-
    sapply(causal_order[name_or_call == "name"], function(x){
      type <- attributes(eval(x))$type
      return(ifelse(!is.null(type), type, "unknown"))
    })
  function_types[name_or_call == "call" & function_types == ""] <- "unknown"

  causal_order_types <- function_types
  causal_order_types[!function_types %in% c("estimand", "estimator")] <-
    "dgp"

  estimand_labels <- sapply(causal_order[function_types == "estimand"], function(x) attributes(x)$label)
  if (length(unique(estimand_labels)) != length(estimand_labels)) {
    stop("You have estimands with identical labels. Please provide estimands with unique labels.")
  }

  estimator_labels <- sapply(causal_order[function_types == "estimator"], function(x) attributes(x)$label)
  if (length(unique(estimator_labels)) != length(estimator_labels)) {
    stop("You have estimators with identical labels. Please provide estimators with unique labels.")
  }

  process_population <- function(population){
    ## the first part of the DGP must be a data.frame. Take what the user creates and turn it into a data.frame.
    if (class(population) == "data.frame") {
      current_df <- population
    } else if (class(population) == "call") {
      try(current_df <- population, silent = TRUE)
      if (!exists("current_df") | class(current_df) != "data.frame") {
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else if (class(population) == "function") {
      try(current_df <- population(), silent = TRUE)
      if (!exists("current_df") | class(current_df) != "data.frame") {
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else {
      stop("The first element of your design must be a data.frame or a function that returns a data.frame.")
    }
    return(current_df)
  }

  # this extracts the "DGP" parts of the causal order and runs them.
  data_function <- function() {

    current_df <- process_population(causal_order[[1]])

    if (length(causal_order) > 1) {

      for (i in 2:length(causal_order)) {

        # if it's a dgp
        if (causal_order_types[i] == "dgp") {

          current_df <- causal_order[[i]](current_df)
        }
      }
    }
    return(current_df)
  }

  # This does causal order step by step; saving calculated estimands and estimates along the way

  design_function <- function() {

    current_df <- process_population(causal_order[[1]])

    estimates_df <- estimands_df <- data.frame()

    if (length(causal_order) > 1) {
      for (i in 2:length(causal_order)) {

        # if it's a dgp
        if (causal_order_types[i] == "dgp") {

          current_df <- causal_order[[i]](current_df)

        } else if (causal_order_types[i] == "estimand") {

          # if it's an estimand
          estimands_df <- bind_rows(estimands_df, causal_order[[i]](current_df))

        } else if (causal_order_types[i] == "estimator") {

          # if it's an estimator
          estimates_df <- bind_rows(estimates_df, causal_order[[i]](current_df))

        }
      }
    }
    return(list(estimates_df = estimates_df, estimands_df = estimands_df, data = current_df))
  }

  summarize_step <- function(last_df = NULL, current_df){
    current_names <- names(current_df)
    if (is.null(last_df)) {
      return(current_names)
    } else {
      return(current_names[!current_names %in% names(last_df)])
    }
  }

  summary_function <- function() {

    variables_added <- quantities_added <- vector("list", length(causal_order))

    current_df <- process_population(causal_order[[1]])

    variables_added[[1]] <- lapply(current_df, describe_variable)

    estimates_df <- estimands_df <- data.frame()

    last_df <- current_df

    if (length(causal_order) > 1) {

      for (i in 2:length(causal_order)) {

        # if it's a dgp
        if (causal_order_types[i] == "dgp") {

          current_df <- causal_order[[i]](last_df)

          variables_added_names <- summarize_step(last_df = last_df, current_df = current_df)

          variables_added[[i]] <- lapply(current_df[, variables_added_names, drop = FALSE], describe_variable)

          if (!is.null(attributes(causal_order[[i]])$summary_function)) {
            quantities_added[[i]] <- capture.output(attributes(causal_order[[i]])$summary_function(last_df))
          }

          last_df <- current_df

        } else if (causal_order_types[i] == "estimand") {

          quantities_added[[i]] <- causal_order[[i]](current_df)

        } else if (causal_order_types[i] == "estimator") {

          # if it's an estimator
          estimates_df <- bind_rows(estimates_df, causal_order[[i]](current_df))

          quantities_added[[i]] <- causal_order[[i]](current_df)

        }
      }
    }
    structure(list(variables_added = variables_added,
                   quantities_added = quantities_added), class = "design_summary")
  }

  return(structure(
    list(
      data_function = data_function,
      design_function = design_function,
      summary_function = summary_function,
      causal_order = causal_order_text,
      causal_order_env = causal_order_env,
      function_types = function_types,
      causal_order_types = causal_order_types,
      meta_design = FALSE,
      call = match.call()
    ),
    class = "design"
  ))

}

#' @export
print.design <- function(x, ...) {
  print(summary(x))
  invisible(summary(x))
}

#' Text Summary of a Design
#'
#' @param object a design object created by \code{\link{declare_design}}
#' @param ... optional arguments to be sent to summary function
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
#' summary(design)
#' @export
summary.design <- function(object, ...) {
  summ <- object$summary_function()
  structure(list(variables_added = summ$variables_added,
                 quantities_added = summ$quantities_added,
                 causal_order = object$causal_order,
                 causal_order_types = object$causal_order_types,
                 function_types = object$function_types),
            class = c("summary.design", "list"))
}

#' @export
print.summary.design <- function(x, ...) {

  cat("\nDesign Summary\n\n")

  for (i in 1:max(length(x$variables_added), length(x$quantities_added))) {
    step_name <- deparse(x$causal_order[[i]])
    step_class <- ifelse(x$function_types[[i]] != "unknown", gsub("_", " ", x$function_types[[i]]), "custom data modification")
    cat("Step ", i, " (", step_class, "): ", step_name, " ", paste0(rep("-", 80 - 11 - nchar(i) - nchar(step_class) - nchar(step_name)), collapse = ""), "\n\n", sep = "")

    if (!is.null(x$quantities_added[[i]])) {
      if (class(x$quantities_added[[i]]) == "data.frame") {
        cat("A single draw of the ", x$function_types[[i]], ":\n", sep = "")
        print(x$quantities_added[[i]], row.names = FALSE)
        cat("\n")
      } else {
        cat(x$quantities_added[[i]], sep = "\n")
        cat("\n")
      }
    }
    if (!is.null(x$variables_added[[i]])) {
      for (j in seq_along(x$variables_added[[i]])) {
        cat("Added variable: ", names(x$variables_added[[i]])[j], "\n")
        print(x$variables_added[[i]][[j]], row.names = FALSE)
        cat("\n")
      }
    }
  }
  invisible(x)
}


