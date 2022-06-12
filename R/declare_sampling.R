#' Declare sampling procedure
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return A sampling declaration, which is a function that takes a data.frame as an argument and returns a data.frame subsetted to sampled observations and (optionally) augmented with inclusion probabilities and other quantities.
#' @export
#'
#' @importFrom rlang quos quo call_modify eval_tidy !!!
#' @importFrom randomizr declare_rs
#'
#' @examples
#'
#'
#'  
#' # declare_sampling in use
#' ## Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # Set up population to sample from
#' model <- declare_model(
#'   villages = add_level(
#'     N = 30, 
#'     N_households = sample(c(50:100), N, replace = TRUE)
#'   ),
#'   households = add_level(
#'     N = N_households, 
#'     N_members = sample(c(1, 2, 3, 4), N, 
#'                        prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
#'   ),
#'   individuals = add_level(
#'     N = N_members, 
#'     age = sample(18:90, N, replace = TRUE),
#'     gender = rbinom(n = N, size = 1, prob = .5)
#'   )
#' )
#' 
#' # Sampling procedures
#' ## Complete random sampling
#' design <- model +
#'   declare_sampling(S = complete_rs(N = N, n = 1000))
#' 
#' ## Cluster random sampling
#' design <- model +
#'   declare_sampling(S = cluster_rs(clusters = villages, 
#'                                   n = 15))
#' 
#' ## Strata and cluster random sampling
#' design <- model +
#'   declare_sampling(S  = strata_and_cluster_rs(
#'     strata = villages,
#'     clusters = households,
#'     strata_n = rep(20, 30)))
#' 
#' ## Stratified random sampling
#' design <- model +
#'   declare_sampling(S = strata_rs(strata = gender, n = 100))
#' 
#'
declare_sampling <- make_declarations(sampling_handler, "sampling")

#' @param legacy Use the legacy randomizr functionality. This will be disabled in future; please use legacy = FALSE.
#' @param data A data.frame.
#' @importFrom rlang quos !!! call_modify eval_tidy quo
#' @importFrom randomizr draw_rs obtain_inclusion_probabilities
#' @rdname declare_sampling
sampling_handler <- function(data, ..., legacy = FALSE) {
  
  options <- quos(...)
  
  if(!legacy) {
    
    options$legacy <- NULL
    
    eval_tidy(quo(sampling_handler_internal_fabricatr(data = data, !!!options)))
    
  } else {
    
    options$legacy <- NULL
    
    eval_tidy(quo(sampling_handler_internal_randomizr(data = data, !!!options)))
    
  }
  
}

sampling_handler_internal_fabricatr <- function(data, ..., filter = S == 1) {
  
  options <- quos(...)
  
  data <- fabricate(data = data, !!!options, ID_label = NA)
  
  rows <- enquo(filter)
  rows_val <- eval_tidy(rows, data)
  # rows_val <- rows_val && !is.na(rows_val)
  rows_val[is.na(rows_val)] <- FALSE
  
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
  
}

sampling_handler_internal_randomizr <- function(data, ..., sampling_variable = "S", drop_nonsampled = TRUE) {
  
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

#' @importFrom rlang as_label
validation_fn(sampling_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)
  
  if(!is.null(eval_tidy(dots[["legacy"]])) && eval_tidy(dots[["legacy"]]) == TRUE) {
    
    if ("sampling_variable" %in% names(dots) &&
        inherits(f_rhs(dots[["sampling_variable"]]), "NULL")) {
      declare_time_error("Must not provide NULL as sampling_variable.", ret)
    }
    
    if (!"declaration" %in% names(dots)) {
      if ("strata" %in% names(dots)) {
        if (inherits(f_rhs(dots[["strata"]]), "character")) {
          declare_time_error("Must provide the bare (unquoted) strata variable name to strata.", ret)
        }
      }
      
      if ("clusters" %in% names(dots)) {
        if (inherits(f_rhs(dots[["clusters"]]), "character")) {
          declare_time_error("Must provide the bare (unquoted) cluster variable name to clusters.", ret)
        }
      }
      rs_args <- setdiff(names(dots), names(formals(sampling_handler_internal_randomizr))) # removes data and sampling_variable
      
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
    
  } else {
    
    randomizr_args <-
      c(
        "strata",
        "clusters",
        "n",
        "n_unit",
        "prob",
        "prob_unit",
        "strata_n",
        "strata_prob",
        "simple"
      )
    
    if(any(randomizr_args %in% names(dots))){
      
      if("sampling_variable" %in% names(dots)){
        sampling_variable <- get_expr(dots[["sampling_variable"]])
      } else {
        sampling_variable <- "S"
      }
      
      args_quos <- dots[names(dots) %in% randomizr_args]
      
      args_list <- lapply(args_quos, as_label)
      
      suggested_call <-
        paste0(
          "declare_sampling(",
          sampling_variable,
          " = draw_rs(N = N, ",
          paste0(
            paste0(names(args_list), " = ", args_list),
            collapse = ", "),
          "), filter = ", sampling_variable, " == 1)")
      
      stop(paste0("You appear to have used legacy declare_sampling() syntax. Consider:\n\n", suggested_call, "\n\nAlternatively, you can set legacy = TRUE to restore the previous functionality.\n\nIf you received this message in error, please ensure that you do not name variables 'strata', 'clusters', 'n', 'n_unit', 'prob', 'prob_unit', 'strata_n', 'strata_prob', or 'simple'."), call. = FALSE)
    } else if (length(names(dots)[!names(dots) %in% "data"]) == 0) { 
      stop(paste0("You appear to have used legacy declare_sampling() syntax. Consider:\n\n", "declare_sampling(Z = complete_rs(N = N))", "\n\nAlternatively, you can set legacy = TRUE to restore the previous functionality."), call. = FALSE)
    }
    
  }
  ret
}
