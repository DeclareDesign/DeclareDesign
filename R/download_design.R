

#' Get Design from the DeclareDesign Library of Research Designs
#'
#' @param name Name of the design
#'
#' @return A design object or template function
#'
#' @export
download_design <- function(name) {
  design_library_url_R <- "https://declaredesign.org/library/"
  design_URL <- paste0(design_library_url_R, name, ".rds")
  data_URL <- paste0(design_library_url_R, name, ".rdata")
  suppressWarnings(try(load(url(data_URL)), silent = TRUE)
  )
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
  design_library_url_R <- "https://declaredesign.org/templates/"
  design_URL <- paste0(design_library_url_R, name, "_template.rds")
  readRDS(gzcon(url(design_URL)))
}
