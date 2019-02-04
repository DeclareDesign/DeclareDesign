#' Compare the code of two designs
#'
#' @param design1 A design object, typically created using the + operator
#' @param design2 A design object, typically created using the + operator
#' @param format Format (in console or HTML) options from \code{diffobj::diffChr}
#' @param mode Mode options from \code{diffobj::diffChr}
#' @param ... Options sent to \code{diffobj::diffChr}
#'
#' @return A Diff object that can be compared using the diffobj package
#' @export
#'
#' @examples
#' @importFrom diffobj diffChr
compare_design_code <- function(design1, design2, format = "ansi8", mode = "sidebyside") {
  
  # design_1_name <- expr_text(enexpr(design1))
  # design_2_name <- expr_text(enexpr(design2))
  
  design1 <- get_design_code(design1)
  design2 <- get_design_code(design2)
  
  structure(diffChr(design1, design2, format = format, mode = mode), class = "Diff", package = "diffobj")
  
}

clean_call <- function(call) {
  paste(sapply(deparse(call), trimws), collapse = " ")
}

get_design_code <- function(design){
  sapply(design, function(x) clean_call(attributes(x)$call))
}

