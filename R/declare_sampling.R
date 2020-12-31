#' Declare sampling procedure
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return A function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#'
#' @importFrom rlang quos quo call_modify eval_tidy !!!
#' @importFrom randomizr declare_rs
#'
#' @examples
#'
#' design <- declare_model(
#' 
#'   classrooms = add_level(10),
#'   individuals = add_level(20, female = rbinom(N, 1, 0.5))
#'
#' ) + NULL
#'
#' # Complete random sampling
#' 
#' design + declare_sampling(S = complete_rs(N = N, n = 50), filter = S == 1)
#'
#' # equivalently, by default filter is set to S == 1
#' design + declare_sampling(S = complete_rs(N = N, n = 50))
#' 
#' # Stratified random sampling
#'
#' design + declare_sampling(S = strata_rs(strata = female))
declare_sampling <- make_declarations(sampling_handler, "sampling")

#' @param filter Unquoted expression for filtering S. By default subsets to \code{S == 1}.
#' @param data A data.frame.
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_sampling
sampling_handler <- function(data, ..., filter = S == 1) {

  options <- quos(...)
  
  data <- fabricate(data = data, !!!options, ID_label = NA)
  
  rows <- enquo(filter)
  rows_val <- eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
  
}

#' @param sampling_variable The prefix for the sampling inclusion probability variable.
#' @param drop_nonsampled Logical indicating whether to drop units that are not sampled. Default is \code{TRUE}.
#' @param data A data.frame.
#' @importFrom rlang quos !!! call_modify eval_tidy quo
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
#' @rdname declare_sampling
sampling_handler_legacy <- function(data, ..., sampling_variable = "S", drop_nonsampled = TRUE) {
  ## draw sample

  options <- quos(...)

  samp <- reveal_nse_helper(sampling_variable)
  samp_inclusion_prob <- as.symbol(paste0(samp, "_inclusion_prob"))

  decl <- eval_tidy(quo(declare_rs(N = !!nrow(data), !!!options)), data)
  
  data <- fabricate(data,
    !!samp := draw_rs(!!decl),
    !!samp_inclusion_prob := obtain_inclusion_probabilities(!!decl),
    ID_label = NA
  )
  
  S <- as.character(as.symbol(samp))

  ## subset to the sampled observations
  if(drop_nonsampled == TRUE) {
    data[ data[[S]] %in% 1, names(data) != S, drop = FALSE]
  } else {
    data
  }
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

        ret <- build_step(currydata(sampling_handler, dots),
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
