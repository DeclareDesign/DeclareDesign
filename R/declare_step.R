#' Declare a custom step
#'
#' With declare_step, you can include any function that takes data as one of its arguments and returns data in a design declaration. The first argument is always a "handler", which is the name of the data-in, data-out function.
#' For handy data manipulations use \code{declare_step(fabricate, ...)}.
#' @inheritParams declare_internal_inherit_params
#' @return A function that returns a data.frame.
#' @export
#' @examples
#'
#' population <- declare_population(N = 5, noise = rnorm(N))
#' manipulate <- declare_step(fabricate, noise_squared = noise^2, zero = 0)
#'
#' design <- population + manipulate
#' draw_data(design)
#'
declare_step <- make_declarations(function(data, ...f, ...) ...f(data, ...), "custom")
