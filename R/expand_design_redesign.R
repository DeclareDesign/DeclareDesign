#' Declare a Design via a Template Function
#'
#' \code{expand_design} easily generates a set of design from a template function.
#'
#' @param template a function which yields a design.
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
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
#' @importFrom rlang quo_expr quos
#' @export
expand_design <- function(template, expand = TRUE, prefix = "design", ...) {

  if (expand) {
    template_args_matrix <- expand.grid(..., stringsAsFactors = FALSE)
  } else { 
    template_args_matrix <- cbind.data.frame(..., stringsAsFactors = FALSE)
  }
  
  k <- nrow(template_args_matrix)
  
  if (k > 0) {
    
    out <- by(template_args_matrix, seq_len(k), do.call, what = template, simplify = FALSE)
    
    # Add attribute
    for (j in seq_len(k)) {
      attr(out[[j]], "parameters") <- setNames(template_args_matrix[j,], names(template_args_matrix))
    }
    
    # if it only produces a single design, return the design object rather than a list of length 1
    if (length(out) == 1) {
      out <- out[[1]]
    } else {
      out <- as(out, "list")
      names(out) <- paste0(prefix, "_", seq_len(length(out)))
    }
    
  } else {
    out <- template()
  }

  return(out)

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
  attributes(design, "code") <- NULL
  design
}

