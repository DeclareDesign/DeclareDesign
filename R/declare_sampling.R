
#' Declare Sampling Procedure
#'
#' @param ... Arguments to the sampling function
#' @param sampling_function A function that takes a data.frame, subsets to sampled observations and optionally adds sampling probabilities or other relevant quantities, and returns a data.frame. By default, the sampling_function uses the \code{randomizr} functions \code{\link{draw_rs}} and \code{\link{obtain_inclusion_probabilities}} to conduct random sampling and obtain the probability of inclusion in the sample.
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#' @details
#'
#' While declare_sampling can work with any sampling_function that takes data and returns data, most random sampling procedures can be easily implemented with randomizr. The arguments to \code{\link{draw_rs}} can include N, strata_var, clust_var, n, prob, strata_n, and strata_prob. The arguments you need to specify are different for different designs. Check the help files for \code{\link{complete_rs}}, \code{\link{strata_rs}}, \code{\link{cluster_rs}}, or \code{\link{strata_and_cluster_ra}} for details on how to execute many common designs.
#' @examples
#'
#' my_population <- declare_population(N = 100, female = rbinom(N, 1, .5))
#' df <- my_population()
#'
#' # Simple random sampling using randomizr
#' # use any arguments you would use in draw_rs.
#'
#' my_sampling <- declare_sampling(n = 50)
#' df <- my_sampling(df)
#' dim(df)
#' head(df)
#'
#' # Stratified random sampling
#' my_stratified_sampling <- declare_sampling(strata_var = female)
#' df <- my_population()
#' table(df$female)
#' df <- my_stratified_sampling(df)
#' table(df$female)
#'
#' # Custom random sampling functions
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
#' df <- my_sampling_custom(df)
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

  if (from_package(sampling_function, "DeclareDesign") &
      substitute(sampling_function) == "sampling_function_default") {

    args_lazy <- lazy_dots(...)

    randomizr_summary <- function(data){
      if (length(args_lazy) > 0) {
        args_lazy$N <- as.lazy(nrow(data), env = args_lazy[[1]]$env)
      } else {
        args_lazy$N <- as.lazy(nrow(data))
      }

      mcall <- make_call(quote(randomizr::declare_rs), args = args_lazy)

      rs_declaration <- lazy_eval(mcall, data = data)

      return(print(rs_declaration))
    }

    attributes(sampling_function_internal)$summary_function <- randomizr_summary

  }

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



