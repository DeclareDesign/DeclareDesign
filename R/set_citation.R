#' Set the citation of a design
#'
#' @param design A design typically created using the + operator
#' @param title The title of the design, as a character string.
#' @param author The author(s) of the design, as a character string.
#' @param year The year of the design, as a character string.
#' @param description A description of the design in words, as a character string.
#' @param citation (optional) The preferred citation for the design, as a character string, in which case title, author, year, and description may be left unspecified.
#'
#' @return a design object with a citation attribute
#'
#' @examples
#'
#' design <-
#' declare_population(data = sleep) +
#'   declare_sampling(n = 10)
#'
#' design <-
#'   set_citation(design,
#'                author = "Lovelace, Ada",
#'                title = "Notes",
#'                year = 1953,
#'                description = "This is a text description of a design")
#'
#' cite_design(design)
#'
#' @export
set_citation <-
  function(design,
             title = NULL,
             author = NULL,
             year = NULL,
             description = "Unpublished research design declaration",
             citation = NULL) {
    if (!is.null(citation)) {
      cite <- citation
    } else {
      cite <- bibentry(
        "unpublished",
        title = title,
        author = author,
        note = description,
        year = year,
        textVersion = citation
      )
    }
    attr(design, "citation") <- cite
    design
  }
