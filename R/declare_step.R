#' Declare a custom step
#' 
#' With declare_step, you can include any function that takes data as one of its arguments and returns data in a design declaration. The first argument is always a "handler", which is the name of the data-in, data-out function. 
#' 
#' @inheritParams declare_internal_inherit_params
#' @return a function that returns a data.frame
#' @export
#' @examples
#'
#' N <- 50
#' my_population <- declare_population(N = N, noise = rnorm(N))
#' my_assignment <- declare_assignment(m = 25)
#' 
#' # use fabricate as the custom step
#' my_step <- declare_step(handler = fabricate, Z2 = Z, q = 5)
#'
#' design <- my_population + my_assignment + my_step
#' 
#' # use dplyr's mutate
#' my_step <- declare_step(handler = dplyr::mutate, Z2 = Z, q = 5)
#'
#' design <- my_population + my_assignment + my_step
#'
declare_step <- make_declarations(fabricate, "custom")