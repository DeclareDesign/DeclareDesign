
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
#' @return a list of two functions, the \code{design_function} and the \code{data_function}. The \code{design_function} runs the design once, i.e. draws the data and calculates any estimates and estimands defined in \code{...}, returned separately as two \code{data.frame}'s. The \code{data_function} runs the design once also, but only returns the final data.
#'
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom dplyr bind_rows
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
#' df <- draw_data(design)
#'
#' estimates <- get_estimates(design)
#' estimands <- get_estimands(design)
#'
#' diagnosis <- diagnose_design(design)
#'
declare_design <- function(...) {

  # Some preprocessing

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
      return(ifelse(!is.null(type), type, "unknown_object"))
    })

  causal_order_types <- function_types
  causal_order_types[!function_types %in% c("estimand", "estimator")] <-
    "dgp"

  estimand_labels <- sapply(causal_order[function_types == "estimand"], function(x) attributes(x)$label)
  if(length(unique(estimand_labels)) != length(estimand_labels)){
    stop("You have estimands with identical labels. Please provide estimands with unique labels.")
  }

  estimator_labels <- sapply(causal_order[function_types == "estimator"], function(x) attributes(x)$label)
  if(length(unique(estimator_labels)) != length(estimator_labels)){
    stop("You have estimators with identical labels. Please provide estimators with unique labels.")
  }


  # this extracts the "DGP" parts of the causal order and runs them.
  data_function <- function() {

    ## the first part of the DGP must be a data.frame. Take what the user creates and turn it into a data.frame.
    if(class(causal_order[[1]]) == "data.frame"){
      current_df <- causal_order[[1]]
    } else if (class(causal_order[[1]]) == "call") {
      try(current_df <- causal_order[[1]], silent = TRUE)
      if(!exists("current_df") | class(current_df) != "data.frame"){
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else if (class(causal_order[[1]]) == "function") {
      try(current_df <- causal_order[[1]](), silent = TRUE)
      if(!exists("current_df") | class(current_df) != "data.frame"){
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else {
      stop("The first element of your design must be a data.frame or a function that returns a data.frame.")
    }

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

    ## the first part of the DGP must be a data.frame. Take what the user creates and turn it into a data.frame.
    if(class(causal_order[[1]]) == "data.frame"){
      current_df <- causal_order[[1]]
    } else if (class(causal_order[[1]]) == "call") {
      try(current_df <- causal_order[[1]], silent = TRUE)
      if(!exists("current_df") | class(current_df) != "data.frame"){
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else if (class(causal_order[[1]]) == "function") {
      try(current_df <- causal_order[[1]](), silent = TRUE)
      if(!exists("current_df") | class(current_df) != "data.frame"){
        stop("The first element of your design must be a data.frame or a function that returns a data.frame. You provided a function that did not return a data.frame.")
      }
    } else {
      stop("The first element of your design must be a data.frame or a function that returns a data.frame.")
    }

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
    return(list(estimates_df = estimates_df, estimands_df = estimands_df))
  }

  return(structure(
    list(
      data_function = data_function,
      design_function = design_function,
      causal_order = causal_order_text,
      call = match.call()
    ),
    class = "design"
  ))

}


