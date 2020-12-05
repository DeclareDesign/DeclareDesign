#' Declare the size and features of the population
#'
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#'
#' # Declare a single-level population with no covariates
#' declare_population(N = 100)
#' 
#' # declare_population returns a function:
#' 
#' my_population <- declare_population(N = 100)
#' #' my_population()
#'
#' # Declare a population from existing data
#' declare_population(sleep)
#'
#' # Declare a single-level population with a covariate
#' declare_population(
#'   N = 6,
#'   female = rbinom(n = N, 1, prob = 0.5),
#'   height_ft = rnorm(N, mean = 5.67 - 0.33 * female, sd = 0.25)
#' )
#' 
#' # Declare a two-level hierarchical population
#' # containing cities within regions and a
#' # pollution variable defined at the city level
#'
#' declare_population(
#'   regions = add_level(N = 5),
#'   cities = add_level(N = 10, pollution = rnorm(N, mean = 5))
#' )
#'
#' # Declare a population using a custom function
#' 
#' the default handler is fabricatr::fabricate, but you can supply any function that returns a data.frame
#'
#' my_population_function <- function(N) {
#'   data.frame(u = rnorm(N))
#' }
#'
#' declare_population(N = 10, handler = my_population_function)
#'
declare_population <- make_declarations(fabricate, "population")
