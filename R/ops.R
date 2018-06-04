

# Slash utility is unreliable
# #' @export
# `/.d_par` <- function(e1,e2) {
#   structure(
#     append(e1, e2),
#     class="design",
#     call=call("/", attr(e1, "call"), attr(e2, "call")))
# }

`%i%` <- intersect

`%||%` <- function(e1, e2) if(is.null(e1)) e2 else e1
