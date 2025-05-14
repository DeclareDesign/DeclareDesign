#' Set elements for the environment of a design
#'
#' A design accesses elements in the global environment that may not be available on later use. Setting these lements saves them into the design step envionments.
#'
#' @param ... Named objects.
#'
#' @return a design object with parameters set in the global environment
#'
#' @examples
#' n = 10
#' design <- declare_model(N = n) + NULL
#' draw_data(design) 
#' design <- set_environment(design, n = 5)
#' rm(n)
#' draw_data(design) 
#' 
#' @export
set_environment <- function(design, ...) {
  
  # check_design_class_single(design)
  
  design <- redesign(design, expand = FALSE, ...)
  # attr(design, "parameters") <- names(list(...))
  design

}
