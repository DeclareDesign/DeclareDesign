#' DeclareDesign package
#'
#' Some stuff about what this package is
#'
#' @docType package
#' @importFrom stats glm lm var vcov sd aggregate anova aov as.formula confint coef df.residual pt qt rbinom rnorm rmultinom update.formula
#' @importFrom utils data
#' @name DeclareDesign
NULL

load <- c("fabricatr", "DDestimate", "randomizr")

.onAttach <- function(...) {
  needed <- load[!is_attached(load)]

  if (length(needed) == 0)
    return()

  packageStartupMessage(paste0("Loading DeclareDesign: ", needed, collapse = "\n"))
  suppressPackageStartupMessages(
    lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  ##declaredesign_conflicts()
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
