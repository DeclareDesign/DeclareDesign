

#' Declare a Design via a Template Function
#'
#' @param template An R function that takes a set of options and returns a design created by declare_design().
#'
#' @param ... Options sent to the template.
#'
#' @importFrom rlang quo_expr quos
#' @export
quick_design <- function(template = NULL, expand = TRUE, ...) {
  # figure out what's in ...
  # dots <- quos(...)
  #
  # args_list <- lapply(dots, function(x) {
  #   e <- as.list(quo_expr(x))
  #   if (length(e) > 1) {
  #     return(e[seq_along(e)[-1]])
  #   } else{
  #     e
  #   }
  # })

  args_list <- list(...)

  if (expand) {
    template_args_matrix <- data.matrix(expand.grid(args_list))
  } else{
    args_lengths <- sapply(args_list, length)
    if (any(args_lengths != args_lengths[1])) {
      stop("If you set expand to FALSE, all arguments you provide must be of the same length.")
    }
    template_args_matrix <- do.call(cbind, args_list)
  }

  template_args_list <- lapply(
    split(template_args_matrix,
          f = row(template_args_matrix)),
    FUN = function(y) {
      x <- as.list(y)
      names(x) <-
        colnames(template_args_matrix)
      x
    }
  )

  designs <- lapply(template_args_list, function(x)
    do.call(template, args = x))

  designs <- list()
  for (i in seq_along(template_args_list)) {
    designs[[i]] <- do.call(template, args = template_args_list[[i]])
    designs[[i]]$characteristics <-
      c(designs[[i]]$characteristics, unlist(template_args_list[[i]]))
  }

  if (length(designs) == 1) {
    designs[[1]]
  } else {
    designs
  }

}
