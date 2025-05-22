#' Redesign
#'
#' \code{redesign} quickly generates a design from an existing one by resetting symbols used in design handler parameters in a step's environment (Advanced).
#'
#' Warning: \code{redesign} will edit any symbol in your design, but if the symbol you attempt to change does not exist in a step's environment no changes will be made and no error or warning will be issued. 
#' 
#' Please note that \code{redesign} functionality is experimental and may be changed in future versions.  
#'
#' @param design An object of class design.
#' @param ... Arguments to redesign e.g., \code{n = 100.} If redesigning multiple arguments, they must be specified as a named list.
#' @param expand If TRUE, redesign using the crossproduct of \code{...}, otherwise recycle them.
#' @return A design, or, in the case of multiple values being passed onto \code{...}, a `by`-list of designs.
#' @examples
#'
#' # Two-arm randomized experiment
#' n <- 500
#' 
#' design <-
#'   declare_model(
#'     N = 1000,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = n)) +
#'   declare_assignment(Z = complete_ra(N = N, m = n/2)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # Use redesign to return a single modified design
#' modified_design <- redesign(design, n = 200)
#' 
#' # Use redesign to return a series of modified designs
#' ## Sample size is varied while the rest of the design remains
#' ## constant
#' design_vary_N <- redesign(design, n = c(100, 500, 900))
#' 
#' \dontrun{
#' # redesign can be used in conjunction with diagnose_designs
#' # to optimize the design for specific diagnosands
#' diagnose_designs(design_vary_N)
#' }
#' 
#' # When redesigning with arguments that are vectors,
#' # use list() in redesign, with each list item
#' # representing a design you wish to create
#' 
#' prob_each <- c(.1, .5, .4)
#' 
#' population <- declare_model(N = 1000)
#' assignment <- declare_assignment(
#'   Z = complete_ra(prob_each = prob_each), 
#'   legacy = FALSE)
#' 
#' design <- population + assignment
#' 
#' ## returns two designs
#' 
#' designs_vary_prob_each <- redesign(
#'   design,
#'   prob_each = list(c(.2, .5, .3), c(0, .5, .5)))
#' 
#' # To illustrate what does and does not get edited by redesign, 
#' # consider the following three designs. In the first two, argument
#' # X is called from the step's environment; in the third it is not.
#' # Using redesign will alter the role of X in the first two designs
#' # but not the third one.
#' 
#' X <- 3
#' f <- function(b, X) b*X
#' g <- function(b) b*X
#' 
#' design1 <- declare_model(N = 1, A = X)       + NULL
#' design2 <- declare_model(N = 1, A = f(2, X)) + NULL
#' design3 <- declare_model(N = 1, A = g(2))    + NULL
#' 
#' draw_data(design1)
#' draw_data(design2)
#' draw_data(design3)
#' 
#' draw_data(redesign(design1, X=0))
#' draw_data(redesign(design2, X=0))
#' draw_data(redesign(design3, X=0))
#' 
#' @export
redesign <- function(design, ..., expand = TRUE) {
  
  check_dots_in_design(design, list(...))
  
  check_design_class_single(design)

  designer <- function(...) par_edit(design, ...)

  design <- expand_design(designer, ..., expand = expand)
  
  structure(design, code = NULL)
}



find_all_objects <- function(design) {
  library(rlang)
  library(purrr)
  library(dplyr)
  library(tibble)
  
  results <- list()
  
  for (step_i in seq_along(design)) {
    step <- design[[step_i]]
    dots <- attr(step, "dots")
    if (is.null(dots)) next
    
    for (quosure_name in names(dots)) {
      q <- dots[[quosure_name]]
      if (!is_quosure(q)) next
      
      q <- as_quosure(q)
      env <- get_env(q)
      
      for (name in ls(env, all.names = TRUE)) {
        val <- tryCatch(get(name, envir = env), error = function(e) "<error>")
        val_str <- tryCatch({
          if (is.atomic(val) && length(val) <= 5) {
            paste0(deparse(val), collapse = "")
          } else if (is.function(val)) {
            "function"
          } else {
            paste0("<", class(val)[1], ">")
          }
        }, error = function(e) "<error>")
        
        results[[length(results) + 1]] <- tibble(
          name = name,
          value_str = val_str,
          step = step_i,
          quosure = quosure_name,
          env = list(env)  # actual environment
        )
      }
    }
  }
  
  bind_rows(results)
}

check_dots_in_design <- function(design, dots) {
  
  missing <- setdiff(names(dots), find_all_objects(design)$name)
  
  if (length(missing) == 1) 
  message(paste("You requested a change to", 
          paste(missing, collapse = ","), 
          "but",  paste(missing, collapse = ","), "is not found in the design"))
  if (length(missing) > 1) 
    message(paste("You requested a change to", 
                  paste(missing, collapse = ","), 
                  "but",  paste(missing, collapse = ","), "are not found in the design"))
  
}


# env_clone <- function(env) {
#   new_env <- new.env(parent = parent.env(env))
#   
#   for (name in ls(env, all.names = TRUE)) {
#     val <- get(name, envir = env)
#     assign(name, val, envir = new_env)
#   }
#   
#   new_env
# }

# MH function to clone environments
clone_design_envs <- function(design) {
  new_design <- design
  
  for (step_i in seq_along(new_design)) {
    step <- new_design[[step_i]]
    dots <- attr(step, "dots")
    
    if (is.null(dots)) next
    
    new_dots <- list()
    for (name in names(dots)) {
      quo <- dots[[name]]
      if (!rlang::is_quosure(quo)) {
        new_dots[[name]] <- quo
        next
      }
      
      old_env <- rlang::get_env(quo)
      new_env <- rlang::env_clone(old_env)
      
      # Reassign the quosure with the new environment
      new_dots[[name]] <- rlang::new_quosure(rlang::get_expr(quo), env = new_env)
    }
    
    attr(step, "dots") <- new_dots
    new_design[[step_i]] <- step
  }
  
  new_design
}


