## ourPkg-deprecated.r
#' @title Deprecated functions in package \pkg{DeclareDesign}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name ourPkg-deprecated
#' @keywords internal
NULL

#' @rdname ourPkg-deprecated
#' @section \code{get_estimands}:
#' For \code{get_estimands}, use \code{\link{draw_estimands}}.
#'
#' @importFrom lifecycle deprecate_stop
#'
#' @export
get_estimands <- function(design) {
  deprecate_stop("1.0.0", "get_estimands()", "draw_estimands()")
}
