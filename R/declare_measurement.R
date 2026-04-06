#' Declare measurement procedure
#'
#' @description Add a step to a design that explicitly indicates what variables are measured by a design.  
#' It is always possible to include measured variables directly in your declare_model calls or using declare_step. 
#' However, putting latent variables in declare_model and variables-as-measured in declare_measurement helps communicate which parts of your research design are in the model and which parts are in the data strategy.
#'
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#' 
#' # declare_measurement a in use a
#' ## Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#'   
#' run_design(design)
#' 
#' # Reveal potential outcomes according to treatment assignment
#' design <-
#'   declare_model(N = 100,
#'                 potential_outcomes(Y ~ rbinom(
#'                   N, size = 1, prob = 0.1 * Z + 0.5
#'                 ))) +
#'   declare_assignment(Z = complete_ra(N, m = 50)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#'   
#' head(draw_data(design))
#' 
#' # Generate observed measurement from a latent value
#' design <- 
#'   declare_model(N = 100, latent = runif(N)) +
#'   declare_measurement(observed = rbinom(N, 1, prob = latent))
#'   
#' head(draw_data(design))
#' 
#' if(require("psych")) {
#' 
#'   # Index creation
#'   
#'   library(psych)
#'   
#'   design <-
#'     declare_model(
#'       N = 500,
#'       X = rep(c(0, 1), each = N / 2),
#'       Y_1 = 0.2 * X + rnorm(N, sd = 0.25),
#'       Y_2 = 0.3 * X + 0.5 * rnorm(N, sd = 0.50),
#'       Y_3 = 0.1 * X + 0.4 * rnorm(N, sd = 0.75)) +
#'     declare_measurement(
#'       index = fa(
#'         r = cbind(Y_1, Y_2, Y_3),
#'         nfactors = 1,
#'         rotate = "varimax"
#'       )$scores
#'     )
#'   
#'   draw_data(design)
#' }
declare_measurement <- make_declarations(measurement_handler, "measurement")

#' @param data A data.frame.
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_measurement
measurement_handler <- function(data, ...) {
  
  options <- quos(...)
  
  fabricate(data = data, !!!options, ID_label = NA)
  
}
