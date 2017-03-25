
#' @importFrom lazyeval lazy_dots make_call lazy_eval
#' @importFrom dplyr bind_rows
#' @export
declare_design <- function(...) {

  causal_order <- eval(substitute(alist(...)))

  causal_order_text <- paste(causal_order)

  name_or_call <- sapply(causal_order, class)

  function_types <- rep("", length(causal_order))

  function_types[name_or_call == "name"] <-
    sapply(causal_order[name_or_call == "name"], function(x){
      type <- attributes(eval(x))$type
      return(ifelse(!is.null(type), type, "unknown_object"))
    })

  causal_order_types <- function_types
  causal_order_types[!function_types %in% c("estimand", "estimator")] <-
    "dgp"

  if (class(eval(causal_order[[1]])) == "function") {
    causal_order_text[1] <- paste0(causal_order_text[1], "()")
  }

  # split into lists that are either 1. dgp chains 2. estimands or 3. estimators
  causal_order_list <-
    split_causal_order(types = causal_order_types,
                       text = causal_order_text)

  causal_order_list_lazy <- list()
  for (i in 1:length(causal_order_list)) {
    causal_order_list_lazy[[i]] <- list()
    for (j in 1:length(causal_order_list[[i]])) {
      causal_order_list_lazy[[i]][[j]] <-
        lazyeval::as.lazy(as.formula(paste0("~", causal_order_list[[i]][j])))
    }
  }

  causal_order_list_lazy_steps <- list()
  for (i in 1:length(causal_order_list)) {
    causal_order_list_lazy_steps[[i]] <-
      lazyeval::as.lazy(as.formula(paste0("~", paste0(causal_order_list[[i]], collapse = " %>% "))))
  }

  # function 1: DGP, returns your "final" dataframe to play with

  # turn each step of the DGP into a pipeline
  data_generating_process_text_list <-
    lazyeval::as.lazy(as.formula(paste0("~", paste(unlist(causal_order_list[names(causal_order_list) == "dgp"]),
                                         collapse = " %>% "))))

  data_function <- function() {
    lazyeval::lazy_eval(data_generating_process_text_list)
  }

  # function 2: runs things in sequence, returns estimates_df

  design_function <- function() {
    # initialize 2 running data.frames
    estimates_df <- estimands_df <- data.frame()

    causal_order_list_types <- names(causal_order_list)

    current_df <-
      lazyeval::lazy_eval(causal_order_list_lazy_steps[[1]])

    if (length(causal_order_list) > 1) {
      for (i in 2:length(causal_order_list)) {
        # 3 cases
        if (causal_order_list_types[i] == "dgp") {
          for (j in 1:length(causal_order_list[[i]])) {
            current_df <- current_df %>% (lazyeval::lazy_eval(causal_order_list_lazy[[i]][[j]]))
          }

        } else if (causal_order_list_types[i] == "estimand") {
          for (j in 1:length(causal_order_list[[i]])) {
            estimands_df <- bind_rows(estimands_df,
                                      current_df %>% (lazyeval::lazy_eval(causal_order_list_lazy[[i]][[j]])))

          }

          # case 3: Estimator
        } else if (causal_order_list_types[i] == "estimator")

          for (j in 1:length(causal_order_list[[i]])) {
            estimates_df <- bind_rows(estimates_df,
                                      current_df %>% (lazyeval::lazy_eval(causal_order_list_lazy[[i]][[j]])))


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

split_causal_order <- function(types, text) {
  rle_out <- rle(types)
  obj <-
    split(text, rep(1:length(rle_out$lengths), rle_out$lengths))
  names(obj) <- rle_out$values
  return(obj)
}

