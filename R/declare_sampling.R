
#' @export
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

#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
sampling_function_default <- function(data, ..., sampling_variable_name = "Z"){

  options <- eval(substitute(alist(...)))
  options$N <- nrow(data)
  data[,sampling_variable_name] <-
    do.call(what = draw_rs, args = options, envir = list2env(data))

  data[,paste0(sampling_variable_name, "_inclusion_prob")] <-
    do.call(what = obtain_inclusion_probabilities, args = options, envir = list2env(data))

  ## subset to the sampled observations and remove the sampling variable
  data[data[, sampling_variable_name] ==1, -which(names(data) %in% sampling_variable_name)]

}

