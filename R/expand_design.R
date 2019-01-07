#' Declare a design via a designer
#'
#' \code{expand_design} easily generates a set of design from a designer function.
#'

#' @param designer a function which yields a design
#' @param ... Options sent to the designer
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
#' @param prefix prefix for the names of the designs, i.e. if you create two designs they would be named prefix_1, prefix_2
#'
#' @return if set of designs is size one, the design, otherwise a `by`-list of designs. Designs are given a parameters attribute with the values of parameters assigned by expand_design.
#'
#' @examples
#' 
#' \dontrun{
#'
#' # in conjunction with DesignLibrary
#' 
#' library(DesignLibrary)
#' 
#' designs <- expand_design(multi_arm_designer, outcome_means = list(c(3,2,4), c(1,4,1)))
#'
#' # with a custom designer function
#' 
#' designer <- function(N) {
#'   pop <- declare_population(N = N, noise = rnorm(N))
#'   pos <- declare_potential_outcomes(Y ~ 0.20 * Z + noise)
#'   assgn <- declare_assignment(m = N / 2)
#'   mand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'   mator <- declare_estimator(Y ~ Z, estimand = mand)
#'   pop + pos + assgn + mand + mator
#' }
#'
#' # returns list of eight designs
#' designs <- expand_design(designer, N = seq(30, 100, 10))
#'
#'  # diagnose a list of designs created by expand_design or redesign
#'  diagnosis <- diagnose_design(designs, sims = 50)
#'
#' # returns a single design
#' large_design <- expand_design(designer, N = 200)
#'
#'  diagnose_large_design <- diagnose_design(large_design, sims = 50)
#' }
#'
#' @export
expand_design <- function(designer, ..., expand = TRUE, prefix = "design") {
  dots_quos <- quos(...)

  if (length(dots_quos) == 0) return(designer())
    
  # transpose
  transp <- function(zx,ix) do.call(mapply, 
                               append(mapply(`[`, zx, ix, SIMPLIFY = FALSE), 
                                      list(FUN = list, SIMPLIFY = FALSE), 
                                      after = 0)
                               )

  args <- list(...)
  args <- lapply(args, function(x) if(is.function(x)) list(x) else x)
  
  ix <- lapply(args, seq_along)
  ix <- if(expand) expand.grid(ix) else data.frame(ix)
  
  designs <- lapply(transp(args, ix), do.call, what = designer)

  args_names <- lapply(dots_quos, expand_args_names)
  
  designs <- mapply(structure, 
                    designs, 
                    parameters = transp(args_names, ix), 
                    SIMPLIFY = FALSE)
  

  if (length(designs) == 1) {
    designs <- designs[[1]]
  } else {
    names(designs) <- paste0(prefix, "_", seq_along(designs))
  }

  designs
}



#' @importFrom rlang quo_squash is_call call_args
expand_args_names <- function(x) {
  x_expr <- quo_squash(x)
  is_list_c <- expr_text(as.list(x_expr)[[1]]) %in% c("c", "list")
  x <- if (is_list_c) call_args(x_expr) 
       else if (is_call(x_expr)) eval_tidy(x) 
       else x_expr
  as.character(x)
}


