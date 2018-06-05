#' Declare a Design via a Template Function
#'
#' \code{expand_design} easily generates a set of design from a template function.
#'
#' @param template a function which yields a design.
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
#' @param prefix prefix for the names of the designs, i.e. if you create two designs they would be named prefix_1, prefix_2
#' @param ... Options sent to the template.
#'
#' @return if set of designs is size one, the design, otherwise a `by`-list of designs. Designs are given a parameters attribute with the values of parameters assigned by expand_design.
#'
#' @examples
#'
#' design_template <- function(N = 100) {
#'   population <- declare_population(N = N)
#'   return(declare_design(population))
#'   }
#'
#' # returns list of eight designs
#' vary_n <- expand_design(design_template, N = seq(30, 100, 10))
#'
#' \dontrun{
#'  # diagnose a list of designs created by expand_design or redesign
#'  diagnose_vary_n <- diagnose_design(vary_n)
#' }
#'
#' # returns a single design
#' large_design <- expand_design(design_template, N = 200)
#'
#' \dontrun{
#'  diagnose_large_design <- diagnose_design(large_design)
#' }
#'
#' my_population <- declare_population(N = 100)
#' my_design <- declare_design(my_population)
#'
#' # returns a single, modified design
#' design_large_N <- redesign(my_design, N = 1000)
#'
#' # returns a list of six modified designs
#' design_vary_N <- redesign(my_design, N = seq(500, 1000, 100))
#' attr(design_vary_N[[1]], "parameters")
#'
#' @export
expand_design <- function(template, expand = TRUE, prefix = "design", ...) {
  
  dots_quos <- quos(...)
  
  if (length(dots_quos) > 0) {
    
    args_list <- expand_args(..., expand = expand)
    args_names <- expand_args_names(!!!dots_quos, expand = expand)
    
    designs <- lapply(args_list, function(x) do.call(template, args = x))
    
    for (i in seq_along(designs)) {
      attr(designs[[i]], "parameters") <- setNames(args_names[i,], names(args_names))  
    }
    
    if (length(designs) == 1){
      designs <- designs[[1]]
    } else {
      names(designs) <- paste0(prefix, "_", seq_len(length(designs)))
    }
    
  } else {
    designs <- template()
  }
  
  return(designs)
  
}


expand_args <- function(..., expand = TRUE) {
  
  dots <- list(...)
  
  if(expand){
    lens <- lapply(dots, function(x) seq_len(length(x)))
    args_positions <- do.call(expand.grid, args = lens)
    args_list <- vector("list", nrow(args_positions))
    for (i in 1:nrow(args_positions)) {
      current_list_row <- vector("list", ncol(args_positions))
      names(current_list_row) <- names(dots)
      for (j in 1:ncol(args_positions)) {
        if (length(dots[[j]]) > 1) {
          current_list_row[[j]] <- dots[[j]][[args_positions[i, j]]]
        } else {
          current_list_row[[j]] <- dots[[j]]
        }
      }
      args_list[[i]] <- current_list_row
    }
  } else {
    args_list <- vector("list", length(dots[[1]]))
    for (i in 1:length(args_list)) {
      current_list_row <- vector("list", length(dots))
      names(current_list_row) <- names(dots)
      for (j in 1:length(dots)) {
        if (length(dots[[j]]) > 1) {
          current_list_row[[j]] <- dots[[j]][j]
        } else {
          current_list_row[[j]] <- dots[[j]]
        }
      }
      args_list[[i]] <- current_list_row
    }
  }
  args_list
}

#' @importFrom rlang quo_squash is_call call_args
expand_args_names <- function(..., expand = TRUE){
  dots_quos <- quos(...)
  
  dots_names <- lapply(dots_quos, function(x) {
    x_expr <- quo_squash(x)
    x_is_call <- is_call(x_expr)
    if (x_is_call) {
      as.character(call_args(x_expr))
    } else {
      as.character(x_expr)
    }
  })
  if(expand){
    ret <- expand.grid(dots_names)
  } else {
    ret <- data.frame(dots_names)
  }
  ret
}





#' Redesign
#' 
#' \code{redesign} quickly generates a design from an existing one by resetting symbols used in design handler parameters internally. (Advanced).
#' 
#' Importantly, \code{redesign} will edit any symbol in your design, but if the symbol you attempt to change does not exist no changes will be made and no error or warning will be issued.
#' 
#' 
#'
#' @param design a design
#' @export
#' @rdname expand_design
redesign <- function(design, expand=TRUE, ...) {
  f <- function(...) clone_design_edit(design, ...)
  design <- expand_design(f, expand, ...)
  structure(design, code = NULL)
}

