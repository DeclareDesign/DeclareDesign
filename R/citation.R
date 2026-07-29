#' Attach a citation to a design (stub)
#'
#' Records bibliographic information on the design. This is currently a
#' lightweight stub.
#'
#' @param design A `design`.
#' @param ... Citation fields (e.g., `title`, `author`, `year`).
#' @return The design with a `citation` attribute set.
#' @export
#' @examples
#' design <- declare_model(N = 10, Y = rnorm(N))
#' set_citation(design, title = "Example", author = "Coppock", year = 2026)
set_citation <- function(design, ...) {
  attr(design, "citation") <- list(...)
  design
}

#' Retrieve a design's citation (stub)
#'
#' @param design A `design`.
#' @return The citation list, or `NULL` if none has been set.
#' @export
#' @examples
#' design <- set_citation(declare_model(N = 10, Y = rnorm(N)),
#'                        title = "Example")
#' cite_design(design)
cite_design <- function(design) {
  attr(design, "citation")
}
