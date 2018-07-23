#' Declare sampling procedure
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return A function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#' @details
#'\code{declare_sampling} can work with any sampling_function that takes data and returns data. The default handler is \code{draw_rs} from the \code{randomizr} package. This allows quick declaration of many sampling schemes that involve strata and clusters.
#'
#'The arguments to \code{\link{draw_rs}} can include N, strata_var, clust_var, n, prob, strata_n, and strata_prob.
#'The arguments you need to specify are different for different designs.
#'
#'Note that \code{declare_sampling} works similarly to \code{declare_assignment} a key difference being that \code{declare_sampling} functions subset data to sampled units rather than simply appending an indicator for membership of a sample (assignment). If you need to sample but keep the dataset use \code{declare_assignment} and define further steps (such as estimation) with respect to subsets defined by the assignment.
#'
#'For details see the help files for \code{\link{complete_rs}}, \code{\link{strata_rs}}, \code{\link{cluster_rs}}, or \code{\link{strata_and_cluster_rs}}
#' @importFrom rlang quos quo lang_modify eval_tidy !!!
#' @importFrom randomizr declare_rs
#'
#' @examples
#'
#' # Default handler is `draw_rs` from `randomizr` package
#' 
#' # Simple random sampling
#' my_sampling <- declare_sampling(n = 50)
#'
#' # Stratified random sampling
#' my_stratified_sampling <- declare_sampling(strata = female)
#'
#' # Custom random sampling functions
#'
#' my_sampling_function <- function(data, n=nrow(data)) {
#'    data[sample(n,n,replace=TRUE), , drop=FALSE]
#' }
#'
#' my_sampling_custom <- declare_sampling(handler = my_sampling_function)
#'
#' my_sampling_custom(sleep)
declare_sampling <- make_declarations(sampling_handler, "sampling")

#' @param sampling_variable The prefix for the sampling inclusion probability variable.
#' @param data A data.frame.
#' @importFrom rlang quos !!! lang_modify eval_tidy quo
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
#' @rdname declare_sampling
sampling_handler <- function(data, ..., sampling_variable = "S") {
  ## draw sample

  options <- quos(...)

  samp <- reveal_nse_helper(sampling_variable)
  samp <- as.symbol(paste0(samp, "_inclusion_prob"))

  S <- as.symbol(".__Sample") # Matching old code but also eliminating the R CMD check warning that .__Sample is a undef/global variable

  data <- fabricate(data,
    !!S := draw_rs(N = N, !!!options),
    !!samp := obtain_inclusion_probabilities(N = N, !!!options),
    ID_label = NA
  )

  S <- as.character(S)

  ## subset to the sampled observations
  data[ data[[S]] %in% 1, names(data) != S, drop = FALSE]
}

validation_fn(sampling_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)


  if ("sampling_variable" %in% names(dots) &&
    inherits(f_rhs(dots[["sampling_variable"]]), "NULL")) {
    declare_time_error("Must not provide NULL as sampling_variable.", ret)
  }

  if (!"declaration" %in% names(dots)) {
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
    rs_args <- setdiff(names(dots), names(formals(sampling_handler))) # removes data and sampling_variable

    rs_dots <- dots[rs_args]

    if (length(rs_dots) > 0) {
      declaration <- tryCatch(eval_tidy(quo(declare_rs(!!!rs_dots))), error = function(e) e)

      if (inherits(declaration, "rs_declaration")) {
        message("Sampling declaration factored out from execution path.")
        dots[rs_args] <- NULL
        dots$declaration <- declaration

        ret <- build_step(currydata(sampling_handler, dots, strictDataParam = attr(ret, "strictDataParam")),
          handler = sampling_handler,
          dots = dots,
          label = label,
          step_type = attr(ret, "step_type"),
          causal_type = attr(ret, "causal_type"),
          call = attr(ret, "call")
        )
      }
    }
  }
  ret
}
