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
#'   declare_model(N = 10, latent = seq(0, 1, length.out = N)) +
#'   declare_measurement(
#'     observed = as.numeric(cut(latent, breaks = seq(0, 1, length.out = 6), include.lowest = TRUE)))
#' 
#' draw_data(design)
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
