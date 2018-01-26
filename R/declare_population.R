#' Declare the Size and Features of the Population
#'
#' @param ... Arguments to the population function
#' @param handler A function that generates a data.frame containing features of the population. By default, the \link{fabricatr} function \code{\link{fabricate}} function is used, which allows you to draw single or multi-level hierarchical population data.
#' @param label a step label
#' @return a function that returns a data.frame
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#'
#' # Declare a single-level population with no covariates
#' my_population <- declare_population(N = 100)
#' head(my_population())
#'
#' # Declare a single-level population with a covariate
#' my_population <- declare_population(
#'   N = 100,
#'   height_ft = runif(N, 3.5, 8)
#' )
#' head(my_population())
#'
#' # Declare a two-level hierarchical population
#' # containing cities within regions
#'
#' pop <- declare_population(
#'  regions = level(N = 5),
#'  cities = level(N = 10, pollution = rnorm(N, mean = 5)))
#' head(my_population())
#'
#' # Custom population functions
#'
#' my_population_function <- function(N) {
#'   data.frame(u = rnorm(N))
#' }
#'
#' my_population_custom <- declare_population(
#'   handler = my_population_function, N = 100)
#'
#' head(my_population_custom())
declare_population <- make_declarations(fabricate, "population", strictDataParam=FALSE);
# declare_population <-
#   function(...,
#            population_function = fabricate) {
#     args <- eval(substitute(alist(...)))
#     env <- freeze_environment(parent.frame())
#     func <- eval(population_function)
#
#     population_function_internal <- function(data = NULL) {
#       if (!is.null(data) & ("data" %in% names(formals(func)))) {
#         args$data <- data
#       }
#       do.call(func, args = args, envir = env)
#     }
#     attributes(population_function_internal) <-
#       list(call = match.call(), type = "population")
#
#     return(population_function_internal)
#   }
