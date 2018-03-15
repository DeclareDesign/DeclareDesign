

#' @export
`/.d_par` <- function(e1,e2) {
  structure(
    append(e1, e2),
    class="design",
    call=call("/", attr(e1, "call"), attr(e2, "call")))
}

