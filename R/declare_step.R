#' Declare a custom step
#' @inheritParams declare_internal_inherit_params
#' @return a function that returns a data.frame
#' @export
#' @importFrom fabricatr fabricate
#' @examples
#'
#' N <- 50
#' my_population <- declare_population(N = N, noise = rnorm(N))
#' my_assignment <- declare_assignment(m = 25)
#' my_step <- declare_step(Z2 = Z, q = 5)
#'
#' design <- my_population + my_assignment + my_step
#'
declare_step <- make_declarations(fabricate, "custom")
