#' Declare the size and features of the population
#'
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#' # declare_model is usually used when concatenating
#' # design elements with `+`
#' 
#' ## Example: Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 250)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # declare_model returns a function:
#' M <- declare_model(N = 100)
#' M()
#' 
#' # Declare a population from existing data
#' M <- declare_model(data = mtcars)
#' 
#' # Resample from existing data
#' M <- declare_model(N = 100, data = mtcars, handler = resample_data)
#' 
#' # Declare a model with covariates: 
#' # observed covariates X1 and X2 and 
#' # unobserved heterogeneity U that each affect 
#' # outcome Y
#' M <- declare_model(
#'   N = 100,
#'   U = rnorm(N),
#'   X1 = rbinom(N, size = 1, prob = 0.5),
#'   X2 = X1 + rnorm(N),
#'   Y = 0.1 * X1 + 0.2 * X2 + 0.1 * X1 * X2 + U
#' )
#' 
#' # We can draw correlated variables using draw_multivariate
#' M <-
#' declare_model(
#'   draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
#'     N = 1000,
#'     mu = c(0, 0),
#'     Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
#'   )))
#'   
#' # Declare potential outcomes model dependent on assignment Z
#' ## Manually
#' M <- 
#'   declare_model(N = 100, 
#'                 Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
#'                 Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
#'   )
#' 
#' ## Using potential_outcomes
#' M <- 
#'   declare_model(N = 100, 
#'                 potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
#'   )
#'   
#'  
#' ## we can draw from a distribution of effect sizes 
#'  M <-
#' declare_model(
#'   N = 100, 
#'   tau = runif(1, min = 0, max = 1), 
#'   U = rnorm(N), 
#'   potential_outcomes(Y ~ tau * Z + U)
#' )
#' 
#' ## we can simulate treatment-by-covariate effect heterogeneity:
#' M <- 
#' declare_model(
#'   N = 100, 
#'   U = rnorm(N), 
#'   X = rbinom(N, 1, prob = 0.5),
#'   potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
#' )
#' 
#' 
#' ## potential outcomes can respond to two treatments:
#' M <- declare_model(
#'   N = 6,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ Z1 + Z2 + U, 
#'                      conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))))
#' 
#' # Declare a two-level hierarchical population
#' # containing varying numbers of individuals within
#' # households and an age variable defined at the individual
#' # level
#' M <- declare_model(
#'   households = add_level(
#'     N = 100, 
#'     N_members = sample(c(1, 2, 3, 4), N, 
#'                        prob = c(0.2, 0.3, 0.25, 0.25), 
#'                        replace = TRUE)
#'   ),
#'   individuals = add_level(
#'     N = N_members, 
#'     age = sample(18:90, N, replace = TRUE)
#'   )
#' )
#' 
#' 
#' ## Panel data have a more complex structure:
#' M <- declare_model(
#' countries = add_level(
#'   N = 196, 
#'   country_shock = rnorm(N)
#' ),
#' years = add_level(
#'   N = 100, 
#'   time_trend = 1:N,
#'   year_shock = runif(N, 1, 10), 
#'   nest = FALSE
#' ),
#' observation = cross_levels(
#'   by = join_using(countries, years),
#'   observation_shock = rnorm(N),
#'   Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
#' )
#' )
#' 
#' 
#' 
#' 
#' # Declare a population using a custom function
#' # the default handler is fabricatr::fabricate,
#' # but you can supply any function that returns a data.frame
#' my_model_function <- function(N) {
#'   data.frame(u = rnorm(N))
#' }
#' 
#' M <- declare_model(N = 10, handler = my_model_function)
declare_model <- make_declarations(fabricate, "model")
