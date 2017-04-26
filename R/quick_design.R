
#' @importFrom lazyeval lazy_dots
#' @export
quick_design <- function(template = NULL, ...){

  # figure out what's in ...
  dots <- lazy_dots(...)

  args_list <- lapply(dots, function(x) {
    e <- as.list(x$expr)
    if (length(e) > 1) {
      return(e[seq_along(e)[-1]])
    } else{
      e
    }
  })

  template_args_matrix <- data.matrix(expand.grid(args_list))

  template_args_list <- lapply(split(template_args_matrix,
                                     f = row(template_args_matrix)),
                               FUN = function(y){
                                 x <- as.list(y)
                                 names(x) <- colnames(template_args_matrix)
                                 x
                               })

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


