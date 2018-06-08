
#' Declare Research Study Citation
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that takes a data.frame as an argument and returns citation data
#' @export
#'
#' @examples
#'
#'  cite <- declare_citation(
#'     title = paste("Reducing intergroup prejudice and conflict using the media:", 
#'     "A field experiment in Rwanda"), 
#'     author = "Paluck, E.L.", 
#'     description = "Journal of Personality and Social Psychology, 96, 574-587.")
#'  design <- cite + NULL
#'  cite_design(design)

declare_citation <- make_declarations(citation_handler, "citation", causal_type = "citation", strictDataParam = FALSE)

#' @param data Do not use
#' @param title (optional) The title of the study, as a character string.
#' @param authors (optional) The authors of the study, as a character string.
#' @param description (optional) A description of the design in words, as a character string, stored alongside the declaration in code.
#' @param timestamp a timestamp to use in the citation
#' @param citation (optional) The preferred citation for the design, as a character string. Either include the full citation in text, or paste a BibTeX entry. If title and authors are specified and you leave citation empty, a BibTeX entry will be created automatically.
#' @rdname declare_citation
citation_handler <- function(data=stop("Don't evaluate data argument"), title=NULL, authors=NULL,
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
