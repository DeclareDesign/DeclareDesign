
`%i%` <- intersect

`%||%` <- function(e1, e2) {
  if (is.null(e1)) {
    e2 
  } else { 
    e1
  }
}