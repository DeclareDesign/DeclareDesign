#' compare_designs
#' 
#' Compare designs. For best results, use a consistent syntax style across designs 
#' (e.g., do not switch between '=' and '<-' for assignment or 
#' include optional parentheses on some designs but not others). 
#' 
#' @param ... A design or set of designs typically created using the + operator, or a \code{data.frame} of simulations, typically created by \code{\link{simulate_design}}. 
#' @param display c("highlights", "all", "none"), where highlights is the default. 
#' @param sort_comparisons Logical: order rows by Jaccard similarity to the first design?
#' @return Invisibly returns list containing requested data frames (overview and/or highlights).
#' @examples
#' d1 <- declare_population(N = 100) +
#' declare_assignment(m = 50) +
#' declare_potential_outcomes(Y ~ rbinom(n = N, size = 1, prob = 0.5 + .1*Z)) +
#' declare_estimand(ATE = 0.3) +
#' declare_estimator(Y ~ Z)
#'d2 <- declare_population(N = 100) +
#'  declare_assignment(m = 50) +
#'  declare_potential_outcomes(Y ~ rpois(n = N, lambda = 0.5 + .1*Z)) +
#'  declare_estimand(ATE = 0.3) +
#'  declare_estimator(glm(Y ~ Z, family = poisson))
#'
#'d3 <- declare_population(N = 200) +
#'  declare_assignment(m = 100) +
#'  declare_potential_outcomes(Y ~ rpois(n = N, lambda = 0.5 + .1*Z)) +
#'  declare_estimand(ATE = 0.3) +
#'  declare_estimator(glm(Y ~ Z, family = poisson))
#'
#'compare_designs(d1, d2, d3)
#'compare_designs(d1, d2, d3, , display = "all")
#'my_comparison <- compare_designs(d1, d2, d3)
#'  
compare_designs <- function(..., display = c("highlights", "all", "none"),
                            sort_comparisons = TRUE){
  
  display <- match.arg(display, c("highlights", "all", "none"))
  
  clean_call <- function(call) {
    paste(sapply(deparse(call), trimws), collapse = " ")
  }
  
  designs <- list(...)
  if (unlist(unique(lapply(designs, class)))[1] != "design") 
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
  
  # ex. if declare_population is used by at least 1 design, retained in overview
  feature_used <- function(feature) as.logical(sum(nchar(feature)))

  overview <- overview[ , unlist(lapply(overview, feature_used))]
  
  all_tokens <- lapply(overview, strsplit, "[[:punct:]]")
  
  equality_comparisons <- matrix(nrow = N_designs, ncol=ncol(overview), 
                                 dimnames=dimnames(overview), FALSE)
  
  for(i in 1:nrow(overview)){
    equality_comparisons[i, ] <- (overview[1, ] == overview[i, ])
  }
  
  jaccard <- function(feature_tokens){ # https://rbshaffer.github.io/_includes/evaluation-measures-textual.pdf
    
    clean <- function(tokens){
      tokens <- tokens[tokens != ""]
      tokens <- tokens[tokens != " "]
      trimws(tokens)
    }
    
    x <- clean(feature_tokens[[1]])
    
    sim <- c(1)
    for(i in 2:length(feature_tokens)){
      y <- clean(feature_tokens[[i]])
      sim[i] <- mean(x %in% y)
    }
    return(sim)
  }
  
  similarity <- lapply(all_tokens, jaccard)
  similarity <- t(do.call(rbind, similarity))
  rownames(similarity) <- design_names
  
  identical_steps <- unique(similarity) == 1 
  
  identical_attributes <- function(a, b){identical(attributes(a), attributes(b))}
  identical_attr_to_design1 <- unlist(lapply(designs, identical_attributes, designs[[1]]))
  
  similarity <- 0.05*identical_attr_to_design1 + .05*equality_comparisons + 0.9*similarity
  
  highlights <- overview[similarity != 1]
  
  if(sort_comparisons)
    overview <- overview[rank(rowMeans(similarity), ties.method = "first"), ]

    if(display == "all") {
      
      cat("\n\nTests for Equality\n\n")
      print(equality_comparisons)
      cat("\n\n")
      
      cat("\n\nOverview\n\n")
      print(overview)
      cat("\n\n")

    }else{
      if(display == "highlights"){
        if(length(highlights) == 0){
          cat("No differences between designs to highlight.\n" )
        }else{
          cat("\n\nHighlights\n\n")
          if(sum(identical_steps)) print(highlights) 
          cat("\n\n")
        }
      }
    }
  
  if(sum(identical_attr_to_design1)){
    
    reference_code <- attributes(designs[[1]])[["code"]]
    code_differences <- list()
    
    for(d in which(!identical_attr_to_design1)){
      
      code_diffs <- setdiff(attributes(designs[[d]])[["code"]], 
                            reference_code)
      elements <- match(code_diffs, attributes(designs[[d]])[["code"]])
      
      tmp <- rbind(reference_code[elements], code_diffs)
      colnames(tmp) <- c(design_names[1], design_names[d])
      code_differences[[design_names[d]]] <- tmp
      
      if(display != "none") print(code_differences[[design_names[d]]])
    } # end for loop 
  }
  
  out <- list(overview = overview, highlights = highlights, similarity = similarity, equality_comparisons = equality_comparisons)
  if(exists("code_differences"))
    out[["code_differences"]] <- code_differences
  return(invisible(out))
}