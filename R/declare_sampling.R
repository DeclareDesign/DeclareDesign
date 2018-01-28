

#' Declare Sampling Procedure
#'
#' @param ... Arguments to the sampling function
#' @param handler A function that takes a data.frame, subsets to sampled observations and optionally adds sampling probabilities or other relevant quantities, and returns a data.frame. By default, the sampling_function uses the \code{randomizr} functions \code{\link{draw_rs}} and \code{\link{obtain_inclusion_probabilities}} to conduct random sampling and obtain the probability of inclusion in the sample.
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#' @details
#'
#' While declare_sampling can work with any sampling_function that takes data and returns data, most random sampling procedures can be easily implemented with randomizr. The arguments to \code{\link{draw_rs}} can include N, strata_var, clust_var, n, prob, strata_n, and strata_prob. The arguments you need to specify are different for different designs. Check the help files for \code{\link{complete_rs}}, \code{\link{strata_rs}}, \code{\link{cluster_rs}}, or \code{\link{strata_and_cluster_rs}} for details on how to execute many common designs.
#'
#' @importFrom rlang quos quo lang_modify eval_tidy !!!
#' @importFrom randomizr declare_rs
#'
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
#' my_stratified_sampling <- declare_sampling(strata = female)
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
#'    handler = my_sampling_function)
#'
#' df <- my_sampling_custom(df)
#' dim(df)
#' head(df)
declare_sampling <- make_declarations(sampling_function_default, "sampling")

# declare_sampling <-
#   function(..., sampling_function = sampling_function_default) {
#     args <- eval(substitute(alist(...)))
#     env <- freeze_environment(parent.frame())
#     func <- eval(sampling_function)
#     if (!("data" %in% names(formals(func)))) {
#       stop("Please choose a sampling_function with a data argument.")
#     }
#     sampling_function_internal <- function(data) {
#       args$data <- data
#       do.call(func, args = args, envir = env)
#     }
#     attributes(sampling_function_internal) <-
#       list(call = match.call(), type = "sampling")
#
#     if (from_package(sampling_function, "DeclareDesign") &
#         substitute(sampling_function) == "sampling_function_default") {
#       args_randomizr <- quos(...)
#
#       if (any(names(args_randomizr) == "sampling_variable_name")) {
#         args_randomizr$sampling_variable_name <- NULL
#       }
#       randomizr_call <- quo(declare_rs(!!! args_randomizr))
#
#       randomizr_summary <- function(data) {
#         randomizr_call <- lang_modify(randomizr_call, N = nrow(data))
#         return(print(eval_tidy(randomizr_call, data = data)))
#       }
#
#       attributes(sampling_function_internal)$summary_function <-
#         randomizr_summary
#
#     }
#
#     return(sampling_function_internal)
#   }

#' @importFrom rlang quos !!! lang_modify eval_tidy quo
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
sampling_function_default <-
  function(data, ..., sampling_variable_name = "S") {
    ## draw sample

    options <- quos(...)

    if (any(names(options) %in% c("strata_var"))) {
      if (class(f_rhs(options[["strata_var"]])) == "character") {
        stop("Please provide the bare (unquoted) strata variable name to strata_var.")
      }
    }
    if (any(names(options) %in% c("clust_var"))) {
      if (class(f_rhs(options[["clust_var"]])) == "character") {
        stop("Please provide the bare (unquoted) cluster variable name to clust_var.")
      }
    }

    sampling_variable_name <- substitute(sampling_variable_name)
    if (!is.null(sampling_variable_name)) {
      sampling_variable_name <- reveal_nse_helper(sampling_variable_name)
    } else {
      stop("Please provide a name for the sampling variable as sampling_variable_name.")
    }

    rs_call <- quo(draw_rs(!!! options))
    rs_call <- lang_modify(rs_call, N = nrow(data))

    data[, sampling_variable_name] <- eval_tidy(rs_call, data = data)

    ## obtain inclusion probabilities

    prob_call <- quo(obtain_inclusion_probabilities(!!! options))
    prob_call <- lang_modify(prob_call, N = nrow(data))

    data[, paste0(sampling_variable_name, "_inclusion_prob")] <-
      eval_tidy(prob_call, data = data)

    ## subset to the sampled observations and remove the sampling variable
    data[data[, sampling_variable_name] %in% 1,-which(names(data) %in% sampling_variable_name), drop = FALSE]

  }

