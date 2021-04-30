#' Redesign
#'
#' \code{redesign} quickly generates a design from an existing one by resetting symbols used in design handler parameters in a step's environment (Advanced).
#'
#' Warning: \code{redesign} will edit any symbol in your design, but if the symbol you attempt to change does not exist in a step's environment no changes will be made and no error or warning will be issued. 
#' 
#' Please note that \code{redesign} functionality is experimental and may be changed in future versions.  
#'
#' @param design An object of class design.
#' @param ... Arguments to redesign e.g., \code{n = 100.} If redesigning multiple arguments, they must be specified as a named list.
#' @param expand If TRUE, redesign using the crossproduct of \code{...}, otherwise recycle them.
#' @return A design, or, in the case of multiple values being passed onto \code{...}, a `by`-list of designs.
#' @examples
#'
#' n <- 500
#' population <- declare_model(N = 1000)
#' sampling <- declare_sampling(S = complete_rs(N, n = n), 
#'                              legacy = FALSE)
#' design <- population + sampling
#'
#' # returns a single, modified design
#' modified_design <- redesign(design, n = 200)
#'
#' # returns a list of six modified designs
#' design_vary_N <- redesign(design, n = seq(400, 900, 100))
#'
#' # When redesigning with arguments that are vectors,
#' # use list() in redesign, with each list item
#' # representing a design you wish to create
#'
#' prob_each <- c(.1, .5, .4)
#'
#' assignment <- declare_assignment(
#'   Z = complete_ra(prob_each = prob_each), 
#'   legacy = FALSE)
#'
#' design <- population + assignment
#'
#' # returns two designs
#'
#' designs_vary_prob_each <- redesign(
#'   design,
#'   prob_each = list(c(.2, .5, .3), c(0, .5, .5)))
#'
#'
#' # To illustrate what does and does not get edited by redesign, 
#' # consider the following three designs. In the first two, argument
#' # X is called from the step's environment; in the third it is not.
#' # Using redesign will alter the role of X in the first two designs
#' # but not the third one.
#'
#'X <- 3
#'f <- function(b, X) b*X
#'g <- function(b) b*X
#'
#'design1 <- declare_model(N = 1, A = X)       + NULL
#'design2 <- declare_model(N = 1, A = f(2, X)) + NULL
#'design3 <- declare_model(N = 1, A = g(2))    + NULL
#'
#'draw_data(design1)
#'draw_data(design2)
#'draw_data(design3)
#'
#'draw_data(redesign(design1, X=0))
#'draw_data(redesign(design2, X=0))
#'draw_data(redesign(design3, X=0))

#' @export
redesign <- function(design, ..., expand = TRUE) {
  check_design_class_single(design)
  
  f <- function(...) {
    clone_design_edit(design, ...)
  }
  design <- expand_design(f, ..., expand = expand)
  structure(design, code = NULL)
}
