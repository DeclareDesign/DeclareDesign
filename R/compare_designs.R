#' compare_designs
#' 
#' Compare designs
#' 
#' @param ... A design or set of designs typically created using the + operator, or a \code{data.frame} of simulations, typically created by \code{\link{simulate_design}}. 
#' @return A data frame comparing key features (design steps).
#' @examples
#' # compare_designs(a, b, c)
#'
compare_designs <- function(..., display=TRUE){
  
  clean_call <- function(call) {
    paste(sapply(deparse(call), trimws), collapse = " ")
  }
  
  designs <- list(...)
  if (unlist(unique(lapply(args, class)))[1] != "design") 
    stop("All objects must be designs (i.e., DeclareDesign objects).")
  design_names <- as.character(as.list(substitute(list(...)))[-1L])
  N_designs <- length(designs)
  
  overview <- data.frame(assignment = vector("character", N_designs), 
                         stringsAsFactors = FALSE)
  overview$population <- ""
  overview$potential_outcomes <- ""
  overview$diagnosands <- ""
  overview$estimates <- ""
  overview$estimators <- ""
  overview$reveal <- ""
  overview$ra <- ""
  overview$call <- ""
  
  rownames(overview) <- design_names
  
  for(d in 1:N_designs){  # adapted from DeclareDesign::print_code
    
    if(is.null(attributes(designs[[d]]$code))){

      for (Step in seq_along(designs[[d]])) {
        # only steps that are not calls within the design call e.g. mutate(q = 5)
        
        if (inherits(attributes(designs[[d]][[Step]])$call, "call")) {
          tmp <- clean_call(attributes(designs[[d]][[Step]])$call)
          o_col <- which(unlist(lapply(colnames(overview), grepl, tmp)))
          overview[d, o_col] <- tmp
        }
      }
      
      tmp <- clean_call(attributes(designs[[d]])$call)
      o_col <- match("call", colnames(overview))
      overview[d, o_col] <- tmp
      
    }else{
      tmp <- attributes(designs[[d]])$code
      o_col <- which(unlist(lapply(colnames(overview), grepl, tmp)))
      overview[d, o_col] <- tmp
    }
    
  }
  
  if(display) print(overview[ , which(unlist(lapply(overview, length)) > 0)])
  return(overview)
}