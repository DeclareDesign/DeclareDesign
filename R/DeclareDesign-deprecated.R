## DeclareDesign-deprecated.r
#' @title Deprecated functions in package \pkg{DeclareDesign}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name DeclareDesign-deprecated
#' @keywords internal
NULL

draw_assignment <- function(design, data = NULL, start = 1, end = length(design)) {
  .Deprecated()
}

draw_sample <- function(design, data = NULL, start = 1, end = length(design)) {
  .Deprecated()
}
