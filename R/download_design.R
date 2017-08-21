

#' Get Design from the DeclareDesign Library of Research Designs
#'
#' @param name Name of the design
#'
#' @return A design object or template function
#'
#' @export
download_design <- function(name) {
  name <- substitute(name)
  design_library_url_R <- "http://library.declaredesign.org/designs/"
  design_URL <- paste0(design_library_url_R, name, ".RDS")
  readRDS(gzcon(url(design_URL)))
}

#' Get Design Template from the DeclareDesign Library of Research Designs
#'
#' @param name Name of the design template
#'
#' @return An R function
#'
#' @export
download_template <- function(name) {
  name <- substitute(name)
  design_library_url_R <- "http://library.declaredesign.org/templates/"
  design_URL <- paste0(design_library_url_R, name, ".RDS")
  readRDS(gzcon(url(design_URL)))
}
