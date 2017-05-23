

#' @export
declare_meta_analysis <- function(designs, type = data, ...){

  # ... is a list of causal_order steps that happen *after* all the designs
  # ususally meta-estimands and meta-estimators but could be mutate etc.

  ##env <- freeze_environment(parent.frame())

  ## below is directly copied from declare_design

  # Some preprocessing

  type <- substitute(type)

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

  data_function <- function(){
    ## run draw_data separately for each design
    data_list <- list()
    for (i in seq_along(designs)) {
      data_list[[i]] <- draw_data(designs[[i]])
    }
    ## then rbind together
    return(data_list %>% bind_rows(.id = "design_ID"))
  }

  design_function <- function(){
    design_output_list <- list()
    for (i in seq_along(designs)) {
      design_output_list[[i]] <- designs[[i]]$design_function()
    }

    meta_data <- lapply(design_output_list, function(i) i$data) %>%
      bind_rows(.id = "design_ID")
    estimates_df <- lapply(design_output_list, function(i) i$estimates_df) %>%
      bind_rows(.id = "design_ID")
    estimands_df <- lapply(design_output_list, function(i) i$estimands_df) %>%
      bind_rows(.id = "design_ID")

    ## start dd function from other file

    if (type == "data") {
      current_df <- meta_data
    } else if (type == "estimates") {
      current_df <- estimates_df
    }

    for (i in seq_along(causal_order)) {

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

    estimates_df$design_ID[is.na(estimates_df$design_ID)] <- "meta_analysis"
    estimands_df$design_ID[is.na(estimands_df$design_ID)] <- "meta_analysis"

    return(list(estimates_df = estimates_df, estimands_df = estimands_df,
                data = current_df))

  }

  return(structure(
    list(
      data_function = data_function,
      design_function = design_function,
      designs = designs,
      causal_order = causal_order_text,
      causal_order_env = causal_order_env,
      function_types = function_types,
      causal_order_types = causal_order_types,
      meta_design = TRUE,
      call = match.call()
    ),
    class = "metadesign"
  ))

}
