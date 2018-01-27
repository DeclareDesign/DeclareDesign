#' Declare a Design via a Template Function
#'
#' @param template An R function that takes a set of options and returns a design created by declare_design().
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
#' @param ... Options sent to the template.
#'
#' @importFrom rlang quo_expr quos
#' @export
quick_design <- function(template = NULL, expand = TRUE, ...) {

  template_args_matrix <- if (expand)
      expand.grid(..., stringsAsFactors = FALSE)
    else
      cbind.data.frame(..., stringsAsFactors=FALSE)


  by(template_args_matrix, 1:nrow(template_args_matrix), do.call, what=template)

}
