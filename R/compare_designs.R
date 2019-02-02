#' Compare the code of two designs
#'
#' @param design1 A design object, typically created using the + operator
#' @param design2 A design object, typically created using the + operator
#' @param ... Options sent to \code{diffobj::diffObj}
#'
#' @return A Diff object that can be compared using the diffobj package
#' @export
#'
#' @examples
#' @importFrom diffobj diffObj
compare_design_code <- function(...) {
  
  designs <- dots_to_list_of_designs(...)
  
  design1 <- unname(sapply(get_design_code(designs[[1]]), toString))
  design2 <- unname(sapply(get_design_code(designs[[2]]), toString))
  
  structure(diffObj(design1, design2), class = "Diff", package = "diffobj")
  
}

clean_call <- function(call) {
  paste(sapply(deparse(call), trimws), collapse = " ")
}

get_design_code <- function(design){
  lapply(design, function(x) clean_call(attributes(x)$call))
}

