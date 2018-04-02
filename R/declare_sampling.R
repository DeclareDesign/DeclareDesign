

#' Declare Sampling Procedure
#'
#' @inheritParams declare_internal_inherit_params
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
declare_sampling <- make_declarations(sampling_handler, "sampling")


#' @importFrom rlang quos !!! lang_modify eval_tidy quo
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
sampling_handler <- function(data, ..., sampling_variable = "S") {
  ## draw sample

  options <- quos(...)

  samp <- reveal_nse_helper(sampling_variable)
  samp <- as.symbol(paste0(samp, "_inclusion_prob"))

  S <- as.symbol(".__Sample") # Matching old code but also eliminating the R CMD check warning that .__Sample is a undef/global variable

  data <- fabricate(data,
    !!S              :=  draw_rs(N=N, !!!options),
    !!samp           :=  obtain_inclusion_probabilities(N=N, !!!options),
    ID_label = NA
  )

  S <- as.character(S)

  ## subset to the sampled observations
  data[ data[[S]] %in% 1, names(data) != S, drop=FALSE]

}

validation_fn(sampling_handler) <- function(ret, dots, label){

  declare_time_error_if_data(ret)


  if(! "declaration" %in% names(dots)) {

    if ("strata" %in% names(dots)) {
      if (class(f_rhs(dots[["strata"]])) == "character") {
        declare_time_error("Must provide the bare (unquoted) strata variable name to strata.", ret)
      }
    }

    if ("clusters" %in% names(dots)) {
      if (class(f_rhs(dots[["clusters"]])) == "character") {
        declare_time_error("Must provide the bare (unquoted) cluster variable name to clusters.", ret)
      }
    }
    rs_args <- setdiff(names(dots), names(formals(sampling_handler))) #removes data and assignment_variable

    rs_dots <- dots[rs_args]

    if(length(rs_dots) > 0) {
      declaration <- tryCatch(eval_tidy(quo(declare_rs(!!!rs_dots))), error=function(e)e)

      if(inherits(declaration, "rs_declaration")) {
        message("Sampling declaration factored out from execution path.")
        dots[rs_args] <- NULL
        dots$declaration <- declaration

        ret <- build_step(currydata(sampling_handler, dots, strictDataParam=attr(ret, "strictDataParam")),
                          handler=sampling_handler,
                          dots=dots,
                          label=label,
                          step_type=attr(ret, "step_type"),
                          causal_type=attr(ret,"causal_type"),
                          call=attr(ret, "call"))


      }
    }


  }




  if ("sampling_variable" %in% names(dots)) {
    if (class(f_rhs(dots[["sampling_variable"]])) == "NULL") {
      declare_time_error("Must not provide NULL as sampling_variable.", ret)
    }
  }

  ret

}
