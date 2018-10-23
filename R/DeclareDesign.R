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
#'   \item{\code{\link{declare_reveal}}}{Reveal outcomes step}
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

# tab completion

comp_env <- list2env(list(completer = NULL), parent = emptyenv());

.onAttach <- function(libname, pkgname){
  
  if(requireNamespace("utils", quietly = FALSE)){
    comp_env$completer <- utils::rc.getOption("custom.completer")
    
    
    rc.options("custom.completer"= DDcompletor)
    
  }
  
}

.onDetach <- function(libPath){
  rc.options(custom.completer = comp_env$completer)
}

# Sneaks around import checks ? see also jimhest/completeme on github
complete_token <- get(".completeToken", asNamespace("utils"))

DDcompletor <- function(CompletionEnv){
  
  on.exit( rc.options("custom.completer" = DDcompletor) )
  rc.options(custom.completer = comp_env$completer)
  ret <- complete_token() #fills in CompletionEnv data
  
  if(is.character(CompletionEnv$fguess) && length(CompletionEnv$fguess) > 0 && !any(grepl("handler", CompletionEnv$linebuffer))){
    FUN <- get0(CompletionEnv$fguess, asNamespace("DeclareDesign"), mode = "function")
    
    if(is.function(FUN) && inherits(FUN, "declaration")){
      
      if(!CompletionEnv$fguess %in% c("declare_assignment", "declare_sampling", "declare_estimator")){ 
        
        h <- eval(formals(FUN)$handler, environment(FUN))
        
        h <- union(names(formals(FUN)), names(formals(h)))
        
      } else if (CompletionEnv$fguess == "declare_assignment") {
        
        h <- setdiff(names(formals(randomizr::conduct_ra)), c("N", "declaration", "permutation_matrix", "check_inputs"))
        
      } else if (CompletionEnv$fguess == "declare_sampling") {
        
        h <- setdiff(names(formals(randomizr::draw_rs)), c("N", "declaration", "check_inputs"))
        
      } else if (CompletionEnv$fguess == "declare_estimator") {
        
        h <- eval(formals(FUN)$handler, environment(FUN))
        
        handler_name <- quo_text(formals(FUN)$handler)
        
        if(handler_name %in% c("model_handler", "estimator_handler")) {
          
          model_args <- names(formals(eval(formals(h)$model)))
          
          h <- union(union(names(formals(FUN)), names(formals(h))), model_args)
          
        } else {
          
          h <- union(names(formals(FUN)), names(formals(h)))
          
        }
        
      }
      
      h <- setdiff(h, "data")
      
      CompletionEnv$comps = paste0(h, "=")
      
      ## TODO port to startsWith
      if(length(CompletionEnv$token) == 1 && nchar(CompletionEnv$token) > 0){
        w <- regexpr(CompletionEnv$token, h) == 1
        if(sum(w) == 1) {
          CompletionEnv$comps = h[w]
          
          return(h[w])
          
        }
      }
      
      return(h)
    }
  }
  
  ret
}

.onLoad <- function(libname, pkgname) {
  repos <- getOption("repos")
  repos["declaredesign"] <- "https://declaredesign.github.io"
  options(repos = repos)
  invisible(repos)
}

utils::globalVariables(c("Y", "Z", "N", "conf.low", "conf.high", "estimate", "estimand", "p.value", "std.error"))
