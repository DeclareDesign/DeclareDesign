
#' Declare Sampling Procedure
#'
#' @param ... Arguments to the sampling function
#' @param sampling_function A function that takes a data.frame, subsets to sampled observations and optionally adds sampling probabilities or other relevant quantities, and returns a data.frame. By default, the sampling_function uses the \code{randomizr} functions \code{\link{draw_rs}} and \code{\link{obtain_inclusion_probabilities}} to conduct random sampling and obtain the probability of inclusion in the sample.
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#' df <- my_population()
#'
#' # Simple random sampling using randomizr
#' # use any arguments you would use in draw_rs.
#'
#' my_sampling <- declare_sampling(n = 50)
#' df <- my_sampling(pop)
#' dim(df)
#' head(df)
#'
#' # Custom random assignment functions
#'
#' df <- my_population()
#'
#' my_sampling_function <- function(data) {
#'    data$S <- rbinom(n = nrow(data),
#'      size = 1,
#'      prob = 0.5)
#'    data[data$S == 1, ]
#'    }
#'
#' my_sampling_custom <- declare_sampling(
#'    sampling_function = my_sampling_function)
#'
#' df <- my_custom_sampling(pop)
#' dim(df)
#' head(df)
declare_sampling <- function(..., sampling_function = sampling_function_default) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(sampling_function)
  if (!("data" %in% names(formals(func)))) {
    stop("Please choose a sampling_function with a data argument.")
  }
  sampling_function_internal <- function(data) {
    args$data <- data
    do.call(func, args = args, envir = env)
  }
  attributes(sampling_function_internal) <-
    list(call = match.call(), type = "sampling")

  return(sampling_function_internal)
}

#' @importFrom lazyeval make_call lazy_eval as.lazy
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
sampling_function_default <- function(data, ..., sampling_variable_name = "S"){

  ## draw sample

  options <- lazy_dots(...)

  if (length(options) > 0) {
    options$N <- as.lazy(nrow(data), env = options[[1]]$env)
  } else {
    options$N <- as.lazy(nrow(data))
  }

  mcall <- make_call(quote(randomizr::draw_rs), args = options)

  data[,sampling_variable_name] <- lazy_eval(mcall, data = data)

  ## obtain inclusion probabilities

  mcall <- make_call(quote(randomizr::obtain_inclusion_probabilities),
                     args = options)

  data[,paste0(sampling_variable_name, "_inclusion_prob")] <-
    lazy_eval(mcall, data = data)

  ## subset to the sampled observations and remove the sampling variable
  data[data[, sampling_variable_name] == 1,
       -which(names(data) %in% sampling_variable_name), drop = FALSE]

}



