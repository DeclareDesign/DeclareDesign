#' Declare a Design via a Template Function
#'
#' \code{quick_design} easily generates a set of design from a template function.
#'
#' @param template a function which yields a design.
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
#' @param ... Options sent to the template.
#'
#' @return if set of designs is size one, the design, otherwise a `by`-list of designs.
#'
#' @examples
#'
#' d_template <- function(N=100) declare_design(declare_population(N=N))
#'
#' vary_n <- quick_design(d_template, N=seq(30, 100, 10))
#'
#' big_d <- quick_design(d_template, N=200)
#'
#' bigger_d <- redesign(big_d, N=1000)
#'
#' little_d <- redesign(big_d, N=seq(5,25,5))
#'
#' \dontrun{
#'  # idea is to diagnose a list
#'   diagnose_design(little_d, )
#' }
#'
#' @importFrom rlang quo_expr quos
#' @export
quick_design <- function(template, expand = TRUE, ...) {

  template_args_matrix <- if (expand)
    expand.grid(..., stringsAsFactors = FALSE)
  else
    cbind.data.frame(..., stringsAsFactors=FALSE)

  k <- nrow(template_args_matrix)
  if(k == 1) return( template(...) )

  by(template_args_matrix, 1:k, do.call, what=template, simplify = FALSE)

}

#'
#' \code{redesign} quickly generates a design from an existing one by resetting design variables internally.
#'
#' @param a design
#' @export
#' @rdname quick_design
redesign <- function(design, expand=TRUE, ...) {
  f <- function(...) clone_design_edit(design, ...)
  quick_design(f, expand, ...)
}

