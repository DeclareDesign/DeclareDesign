#' DeclareDesign package
#'
#' \begin{enumerate}
#' \item \code{\link{declare_assignment}}
#' \end{enumerate}
#'
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
