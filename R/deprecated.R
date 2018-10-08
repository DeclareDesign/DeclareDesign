
#' Deprecated functions
#'
#' The function \code{get_estimands} has been replaced with \code{draw_estimands}.
#'
#' @param ... options sent to the old version of \code{get_estimands}.
#'
#' @export
#'
#' @name deprecated
get_estimands <- function(...) {
  qs <- quos(...)
  
  .Deprecated(
    new = "draw_estimands",
    old = "get_estimands",
    msg = paste0(
      "\nThe get_estimands function has been renamed draw_estimands, to make clear ",
      "that the estimands are draws from a simulation. Please replace your code with draw_estimands."
    )
  )
  
  cat()
}
