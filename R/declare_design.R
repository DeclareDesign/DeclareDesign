
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom dplyr bind_rows
#' @export
declare_design <- function(...) {


  # Some preprocessign

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


