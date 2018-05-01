#' Declare a custom step
#'
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_assignment,
#'                          my_step)
#'
declare_step <- make_declarations(fabricate, "custom")
