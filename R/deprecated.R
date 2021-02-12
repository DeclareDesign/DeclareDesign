## ourPkg-deprecated.r
#' @title Deprecated functions in package \pkg{DeclareDesign}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name ourPkg-deprecated
#' @keywords internal
NULL

estimand_handler <- function(...){
  .Deprecated(new = "inquiry_handler")
}

compare_design_estimands <- function(...){
  .Deprecated(new = "compare_design_inquiries")
}