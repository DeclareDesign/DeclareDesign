#' DeclareDesign package
#'
#' The four main types of functions are to declare a step, to combine steps into designs,
#' and to manipulate designs and designers (functions that return designs).
#'
#' @section Design Steps:
#' \describe{
#'   \item{\code{\link{declare_population}}}{Population step}
#'   \item{\code{\link{declare_potential_outcomes}}}{Potential outcomes step}
#'   \item{\code{\link{declare_sampling}}}{Sampling step}
#'   \item{\code{\link{declare_assignment}}}{Assignment step}
#'   \item{\code{\link{reveal_outcomes}}}{Reveal outcomes step}
#'   \item{\code{\link{declare_estimand}}}{Estimand step}
#'   \item{\code{\link{declare_estimator}}}{Estimator step}
#' }
#'
#' @section Design Objects:
#' \describe{
#'   \item{+}{Add steps to create a design}
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
#'   \item{\code{\link{redesign}}}{Modify local variables within a design (advanced)}
#' }
#'
#'
#' @section Designers:
#' \describe{
#'   \item{\code{\link{expand_design}}}{Generate designs from a designer}
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
  repos <- getOption("repos")
  repos["declaredesign"] <- "https://declaredesign.github.io"
  options(repos = repos)
  invisible(repos)
}

utils::globalVariables(c("Y", "Z", "N", "conf.low", "conf.high", "estimate", "estimand", "p.value", "std.error"))
