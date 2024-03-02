#' DeclareDesign package
#'
#' The four main types of functions are to declare a step, to combine steps into designs,
#' and to manipulate designs and designers (functions that return designs).
#'
#' @section Design Steps:
#' \describe{
#'   \item{\code{\link{declare_model}}}{Model step}
#'   \item{\code{\link{declare_inquiry}}}{Inquiry step}
#'   \item{\code{\link{declare_sampling}}}{Data strategy step (sampling)}
#'   \item{\code{\link{declare_assignment}}}{Data strategy step (assignment)}
#'   \item{\code{\link{declare_measurement}}}{Data strategy step (measurement)}
#'   \item{\code{\link{declare_estimator}}}{Answer strategy step (Estimator)}
#'   \item{\code{\link{declare_test}}}{Answer strategy step (Testing function)}
#' }
#'
#' @section Design Objects:
#' \describe{
#'   \item{+}{Add steps to create a design}
#'   \item{\code{\link{redesign}}}{Change design parameters}
#'   \item{\code{\link{draw_data}}}{Draw a simulated dataset}
#'   \item{\code{\link{run_design}}}{Draw one set of inquiry values and estimates}
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
"_PACKAGE"

utils::globalVariables(c("Y", "Z", "S", "N", "conf.low", "conf.high", "estimate", "inquiry", "p.value", "std.error", "term"))
