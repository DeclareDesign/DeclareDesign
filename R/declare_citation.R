
#' Declare Research Study Citation
#'
#' @param ... Arguments to the citation function
#' @param handler A function that takes a data.frame and yield citation data
#'
#' @return a function that takes a data.frame as an argument and returns citation data
#' @export
#' @details
#'

declare_citation <- make_declarations(citation_function_default, "citation", causal_type="citation", strictDataParam=FALSE)

citation_function_default <- function(data=stop("Don't evaluate data argument"), title=NULL, authors=NULL,
                                      description = "Unpublished research design declaration.",
                                      timestamp = Sys.time(), citation=NULL) {
  if("bibentry" %in% class(citation)) return(citation)
  if (!is.null(title) && !is.null(authors)) {
    citation <- bibentry(
      "unpublished",
      title = title,
      author = authors,
      note = description,
      month = format(timestamp, "%b"),
      year = format(timestamp, "%Y"),
      textVersion = citation
    )
  }

  citation
}
