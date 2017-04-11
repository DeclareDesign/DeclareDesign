
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

#' @importFrom lazyeval make_call lazy_eval as.lazy
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
sampling_function_default <- function(data, ..., sampling_variable_name = "S"){

  ## draw sample

  options <- lazy_dots(...)
  options$N <- as.lazy(nrow(data), env = options[[1]]$env)

  mcall <- make_call(quote(randomizr::draw_rs), args = options)

  data[,sampling_variable_name] <- lazy_eval(mcall, data = data)

  ## obtain inclusion probabilities

  mcall <- make_call(quote(randomizr::obtain_inclusion_probabilities), args = options)

  data[,paste0(sampling_variable_name, "_inclusion_prob")] <- lazy_eval(mcall, data = data)

  ## subset to the sampled observations and remove the sampling variable
  data[data[, sampling_variable_name] ==1, -which(names(data) %in% sampling_variable_name)]

}



