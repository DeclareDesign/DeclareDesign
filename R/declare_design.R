
#' @importFrom lazyeval lazy_dots make_call lazy_eval as.lazy
#' @importFrom dplyr bind_rows
#' @export
declare_design <- function(...) {

  causal_order <- lazy_eval(lazy_dots(..., .follow_symbols = TRUE))

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



  data_function <- function() {
    current_df <- causal_order[[1]]
    if(length(causal_order) > 1){

      for (i in 2:length(causal_order)) {

        # if it's a dgp
        if (causal_order_types[i] == "dgp") {

          current_df <- causal_order[[i]](current_df)
        }
      }
    }
    return(current_df)
  }

  # function 2: runs things in sequence, returns estimates_df

  design_function <- function() {
    # initialize 3 running data.frames
    current_df <- causal_order[[1]]
    estimates_df <- estimands_df <- data.frame()

    if(length(causal_order) > 1){
      for (i in 2:length(causal_order)) {

        # if it's a dgp
        if(causal_order_types[i] == "dgp") {

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


# # split into lists that are either 1. dgp chains 2. estimands or 3. estimators
# causal_order_list <-
#   split_causal_order(types = causal_order_types,
#                      text = causal_order_text)
# split_causal_order <- function(types, text) {
#   rle_out <- rle(types)
#   obj <-
#     split(text, rep(1:length(rle_out$lengths), rle_out$lengths))
#   names(obj) <- rle_out$values
#   return(obj)
# }
#
