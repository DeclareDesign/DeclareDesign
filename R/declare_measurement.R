#' Declare measurement procedure
#'
#' This function adds measured data columns that can be functions of unmeasured data columns.
#' 
#' It is also possible to include measured variables in your declare_population call or to add variables using declare_step. However, putting latent variables in declare_population and variables-as-measured in declare_measurement helps communicate which parts of your research design are in M and which parts are in D.
#'
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @importFrom fabricatr fabricate
#'
#' @examples
#' 
#' design <-
#'   declare_model(N = 6,
#'                 U = rnorm(N),
#'                 potential_outcomes(Y ~ Z + U)) +
#'   declare_assignment(Z = complete_ra(N), legacy = FALSE) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z))
#' 
#' draw_data(design)
#' 
#' 
#' design <-
#'   declare_model(
#'     N = 6,
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ Z1 + Z2 + U, 
#'                        conditions = list(Z1 = c(0, 1), Z2 = c(0, 1)))) +
#'   declare_assignment(Z1 = complete_ra(N), 
#'                      Z2 = block_ra(blocks = Z1),
#'                      legacy = FALSE) + 
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z1 + Z2))
#' 
#' draw_data(design)
#' 
#' 
#'
declare_measurement <- make_declarations(measurement_handler, "measurement")

#' @param data A data.frame.
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_measurement
measurement_handler <- function(data, ...) {
  
  options <- quos(...)
  
  fabricate(data = data, !!!options, ID_label = NA)
  
}
