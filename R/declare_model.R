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
#' declare_model(N = 100)
#' 
#' # declare_model returns a function:
#' 
#' my_model <- declare_model(N = 100)
#' my_model()
#' 
#' # Declare a single-level population with two covariates
#' declare_model(
#'   N = 6,
#'   female = rbinom(n = N, 1, prob = 0.5),
#'   height_ft = rnorm(N, mean = 5.67 - 0.33 * female, sd = 0.25)
#' )
#' 
#' # Declare a single-level population with potential outcomes
#' declare_model(
#'   N = 6, 
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ Z + U))
#' 
#' 
#' # Declare a single-level population with two sets of potential outcomes
#' declare_model(
#'   N = 6,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ Z1 + Z2 + U, 
#'                      conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))))
#' 
#' 
#' 
#' 
#' # Declare a population from existing data
#' 
#' declare_model(mtcars)
#' 
#' # Resample from existing data
#' 
#' declare_model(N = 100, data = mtcars, handler = resample_data)
#' 
#' 
#' # Declare a two-level hierarchical population
#' # containing cities within regions and a
#' # pollution variable defined at the city level
#' 
#' declare_model(regions = add_level(N = 5),
#'               cities = add_level(N = 10, pollution = rnorm(N, mean = 5)))
#' 
#' # Declare a population using a custom function
#' 
#' # the default handler is fabricatr::fabricate,
#' # but you can supply any function that returns a data.frame
#' 
#' my_model_function <- function(N) {
#'   data.frame(u = rnorm(N))
#' }
#' 
#' declare_model(N = 10, handler = my_model_function)
declare_model <- make_declarations(fabricate, "model")
