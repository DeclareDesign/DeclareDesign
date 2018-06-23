
`%i%` <- intersect

`%icn%` <- function(e1, e2) e1 %i% colnames(e2)


`%||%` <- function(e1, e2) if(is.null(e1)) e2 else e1
