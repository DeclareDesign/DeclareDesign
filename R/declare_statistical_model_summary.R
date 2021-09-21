
#' @export
declare_statistical_model_summary <- make_declarations(statistical_model_summary_handler, step_type = "estimator", causal_type = "estimator")

#' @importFrom rlang eval_tidy new_data_mask as_label
#' @export
statistical_model_summary_handler <- function(data, ..., subset = NULL, inquiry = NULL) {
  dots <- quos(...)
  
  if(length(dots) > 1) {
    stop("Please only send one model summary to declare_statistical_model_summary.")
  }
  
  expr_quo <- dots[[1]]
  
  # label estimator
  nm <- names(dots)[1]
  if(nm == "") {
    nm <- as_label(expr_quo)
  }
  
  estimates_df <- eval_tidy(expr_quo)
  if(!is.null(inquiry)) {
    estimates_df$inquiry_label <- inquiry
  }
  
  subset <- substitute(subset)
  if (is_quosure(subset)) subset <- quo_get_expr(subset)
  idx <- eval_tidy(subset, data = estimates_df)
  if (!is.null(idx)) {
    estimates_df <- estimates_df[idx, , drop = FALSE]
  }
  estimates_df$estimator_label <- nm
  estimates_df
}
