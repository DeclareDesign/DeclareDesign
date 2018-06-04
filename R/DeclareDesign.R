#' DeclareDesign package
#'
#' The four main types of functions are to declare a step, combine steps into designs,
#' and manipulate designs and design templates.
#'
#' @section Design Steps:
#' \describe{
#'   \item{\code{\link{declare_population}}}{Population step}
#'   \item{\code{\link{declare_potential_outcomes}}}{Potential Outcomes step}
#'   \item{\code{\link{declare_sampling}}}{Sampling step}
#'   \item{\code{\link{declare_assignment}}}{Assignment step}
#'   \item{\code{\link{declare_reveal}}}{Reveal Outcomes step}
#'   \item{\code{\link{declare_estimand}}}{Estimand step}
#'   \item{\code{\link{declare_estimator}}}{Estimator step}
#'   \item{\code{\link{declare_citation}}}{Citation step}
#' }
#'
#' @section Design Objects:
#' \describe{
#'   \item{\code{\link{declare_design}}}{Declare a design from steps}
#'   \item{\code{\link{draw_data}}}{Simulate the DGP}
#'   \item{\code{\link{run_design}}}{Simulate the DGP with estimands/estimators}
#'   \item{\code{\link{diagnose_design}}}{Diagnose a design}
#'   \item{\code{\link{cite_design}}}{Cite a design}
#' }
#'
#'
#' @section Design Editing:
#' \describe{
#'   \item{\code{\link{modify_design}}}{Add, delete or replace a step}
#'   \item{\code{\link{redesign}}}{Redeclare local variables within a design (advanced)}
#' }
#'
#'
#' @section Designers:
#' \describe{
#'   \item{\code{\link{expand_design}}}{Generate Designs from a Designer}
#'   \item{designs}{See also the \code{DesignLibrary} package for designers to use}
#' }
#'
#'
#' @docType package
#' @importFrom stats glm lm var vcov sd aggregate anova aov as.formula confint coef df.residual pt qt rbinom rnorm rmultinom update.formula
#' @importFrom utils data capture.output
#' @name DeclareDesign
NULL


.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["declaredesign"] = "https://declaredesign.github.io"
  options(repos = repos)
  invisible(repos)
}

utils::globalVariables(c("Y", "Z", "N"))
