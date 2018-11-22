#' compare_designs
#' 
#' Compare designs. For best results, use a consistent syntax style across designs 
#' (e.g., do not switch between '=' and '<-' for assignment or 
#' include optional parentheses on some designs but not others). 
#' 
#' @param ... A design or set of designs typically created using the + operator, or a \code{data.frame} of simulations, typically created by \code{\link{simulate_design}}. 
#' @param display c("highlights", "all", "none"), where highlights is the default. 
#' @param sort_comparisons Logical: order rows by Jaccard similarity to the first design?
#' @param Rmd_file_prefix Optional. If provided, creates Rmd template based on display type. E.g., "my_comparison".
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
#'my_comparison <- compare_designs(d1, d2, d3, Rmd_file_prefix = "my_report")
#'
#'# Do not change what helper functions do mid-comparison.
#'# For example, don't:
#'g <- function(X) 2*X
#'d1 <- declare_population(N = 2, X = 1:2, Y = g(X)) + NULL
#'g <- function(X) 3*X
#'d2 <- declare_population(N = 2, X = 1:2, Y = g(X)) + NULL
#'compare_designs(d1, d2)
#'# > compare_designs(d1, d2)
# No differences between designs to highlight.
#'# The above misses the key difference. Instead, do:
#'g1 <- function(X) 2*X
#'d1 <- declare_population(N = 2, X = 1:2, Y = g1(X)) + NULL
#'g2 <- function(X) 3*X
#'d2 <- declare_population(N = 2, X = 1:2, Y = g2(X)) + NULL
#'compare_designs(d1, d2)
#'# Highlights
#'# Differences detected between steps:
#'# [1] "declare_population(N = 2, X = 1:2, Y = g2(X))"
#'@export   
compare_designs <- function(..., display = c("highlights", "all", "none"),
                            sort_comparisons = TRUE, Rmd_file_prefix = NULL){
  
  display <- match.arg(display, c("highlights", "all", "none"))
  
  designs <- list(...)
  if (unlist(unique(lapply(designs, class)))[1] != "design") 
    stop("All objects must be designs (i.e., DeclareDesign objects).")
  design_names <- as.character(as.list(substitute(list(...)))[-1L])
  reference_design <- design_names[1]
  N_designs <- length(designs)
  
  # data shape
  data_shape <- t(sapply(designs, function(j) dim(draw_data(j))))
  rownames(data_shape) <- design_names
  colnames(data_shape) <- c("rows", "cols")
  attr(data_shape, "description") <- "Dimensionality of drawn data"
  attr(data_shape, "differences")  <- sum(matrix_difference(t(data_shape)))>0
  attr(data_shape, "highlight")   <- TRUE
  
  # Steps per design
  steps_per_design <- lapply(designs, length)
  names(steps_per_design) <- design_names
  steps_per_design <- do.call(rbind, steps_per_design)
  colnames(steps_per_design) <- "Steps per Design"
  attr(steps_per_design, "description") <- "Number of steps, for each design"
  attr(steps_per_design, "differences")  <- difference(steps_per_design)
  attr(steps_per_design, "highlight")   <- TRUE
  
  # Characters per step
  get_nchar <- function(design) data.frame(t(nchar(lapply(design, attr, "call"))))
  overview_nchar <- lapply(designs, get_nchar) 
  overview_nchar <- rbind_disjoint(overview_nchar)
  row.names(overview_nchar) <- design_names
  attr(overview_nchar, "description") <- "Number of characters per step, for each design"
  attr(overview_nchar, "differences")  <- sum(matrix_difference(overview_nchar))>0
  attr(overview_nchar, "highlight")   <- FALSE

  # Overview
  overview <- data.frame(assignment = rep("", N_designs), 
                         population = "", 
                         potential_outcomes = "",
                         diagnosands = "",
                         estimates = "",
                         estimators = "",
                         reveal = "",
                         ra = "",
                         call = "",
                         stringsAsFactors = FALSE)
  row.names(overview) <- design_names
  
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
  
  overview <- overview[ , unlist(lapply(overview, feature_used))]
  
  all_tokens <- lapply(overview, strsplit, "[[:punct:]]")
  
  # Equality comparison
  equality_comparisons <- matrix(nrow = N_designs, ncol = ncol(overview), 
                                 dimnames = dimnames(overview), FALSE)
  
  for(i in 1:nrow(overview)){
    equality_comparisons[i, ] <- (overview[1, ] == overview[i, ])
  }

  # Similarity
  similarity <- lapply(all_tokens, jaccard)
  similarity <- t(do.call(rbind, similarity))
  rownames(similarity) <- design_names
  
  # Identical Steps
  identical_steps <- unique(similarity) == 1 
  identical_attr_to_design1 <- unlist(lapply(designs, identical_attributes, designs[[1]]))
  
  similarity <- 0.05*identical_attr_to_design1 + .05*equality_comparisons + 0.9*similarity
  
  highlights <- overview[similarity != 1]

  # overview housekeeping  
  if(sort_comparisons)
    overview <- overview[rank(rowMeans(similarity), ties.method = "first"), ]

  
  # Code comparison
  get_d_code <- function(d) {
    d_code <- data.frame(t(paste(lapply(d, attr, "call"))))
    names(d_code) <- names(d)
    d_code}
  
  all_code  <- rbind_disjoint(lapply(designs, get_d_code))
  code_different <- apply(all_code, 2, difference)
  if(any(code_different)) {
    code_differences <- t(all_code[, code_different])    
    colnames(code_differences) <- design_names
    } else {
    code_differences < "No code differences"}

  
  # Bringing it all together
  
  out <- list(design_names = design_names,
              character_comparisons = overview_nchar,
              overview = overview, 
              reference_design = reference_design, 
              highlights = highlights, 
              similarity = similarity, 
              equality_comparisons = equality_comparisons, 
              N_designs = N_designs, steps_per_design = steps_per_design,
              identical_steps = identical_steps, 
              code_differences = code_differences,
              data_shape = data_shape
              )
  
  # Report approach (to be folded into print method)
  for(j in out) {
    if(!is.null(attr(j, "highlight"))) {
    if(attr(j, "highlight") | display =="all"){
    print(paste0("Comparison: ", attr(j, "description"), ". Differences? ", attr(j, "difference")))
  }}}
  
  ## Return
  class(out) <- "design_comparison"
  if(display != "none")
    print(out, display = display, Rmd_file_prefix = Rmd_file_prefix)
  return(invisible(out))
}


## compare_design() helper functions

clean_call <- function(call) paste(sapply(deparse(call), trimws), collapse = " ")
# ex. if declare_population is used by at least 1 design, retained in overview
feature_used <- function(feature) as.logical(sum(nchar(feature)))

identical_attributes <- function(a, b) identical(attributes(a), attributes(b))

difference <- function(x) length(unique(x)) > 1
matrix_difference <- function(x) apply(x, 2, difference)

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

