
#' Deprecated functions
#'
#' @param ... options sent to the old version of \code{declare_design}.
#'
#' @name DeclareDesign-deprecated
#' @section For \code{declare_design}:
#' \code{declare_design}, instead use the \code{+} operator to create a design, i.e. \code{pop + smp + assgn}.
#' @export
#'
declare_design <- function(...) {
  qs <- quos(...)

  .Deprecated(
    new = "+",
    old = "declare_design",
    msg = paste0(
      "\nDeclareDesign no longer includes the declare_design() function from development versions. ",
      "Please use the + operator to create designs. For your design, you can try:\n\n",
      paste(sapply(qs, quo_text), collapse = " + ")
    )
  )

  cat()
}
