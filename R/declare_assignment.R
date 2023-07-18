#' Declare Data Strategy: Assignment
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return A function that takes a data.frame as an argument and returns a data.frame with assignment columns appended.
#' @export
#'
#' @examples
#' # declare_assignment in use
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
#' run_design(design)
#' 
#' # Set up population to assign
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
#' # Assignment procedures
#' ## Complete random assignment
#' design <-
#'   model +
#'   declare_assignment(Z = complete_ra(N = N, m = 1000))
#'   
#' head(draw_data(design))
#' 
#' ## Cluster random assignment
#' design <-
#'   model +
#'   declare_assignment(Z = cluster_ra(clusters = villages,
#'                                     n = 15))
#'                                     
#' head(draw_data(design))
#' 
#' ## Block and cluster random assignment
#' design <-
#'   model +
#'   declare_assignment(Z  = block_and_cluster_ra(
#'     blocks = villages,
#'     clusters = households,
#'     block_m = rep(20, 30)
#'   ))
#' 
#' head(draw_data(design))
#' 
#' ## Block random assignment
#' design <-
#'   model +
#'   declare_assignment(Z = block_ra(blocks = gender, m = 100))
#'   
#' head(draw_data(design))
#' 
#' ## Block random assignment using probabilities
#' design <-
#'   model +
#'   declare_assignment(Z = block_ra(blocks = gender,
#'                                   block_prob = c(1 / 3, 2 / 3)))
#' 
#' head(draw_data(design))
#' 
#' ## Factorial assignment
#' design <-
#'   model +
#'   declare_assignment(Z1 = complete_ra(N = N, m = 100),
#'                      Z2 = block_ra(blocks = Z1))
#' 
#' head(draw_data(design))
#' 
#' ## Assignment using functions outside of randomizr
#' design <-
#'   model +
#'   declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))
#' 
#' head(draw_data(design))
#' 
declare_assignment <- make_declarations(assignment_handler, "assignment")

#' @importFrom rlang quos !!! call_modify eval_tidy quo f_rhs 
#' @importFrom randomizr conduct_ra obtain_condition_probabilities declare_ra
#' @param legacy Use the legacy randomizr functionality. This will be disabled in future; please use legacy = FALSE.
#' @param data A data.frame.
#' @rdname declare_assignment
assignment_handler <- function(data, ..., legacy = FALSE) {
  options <- quos(...)
  
  if(!legacy) {
    
    options$legacy <- NULL
    
    eval_tidy(quo(assignment_handler_internal_fabricatr(data = data, !!!options)))
    
  } else {
    
    options$legacy <- NULL
    
    eval_tidy(quo(assignment_handler_internal_randomizr(data = data, !!!options)))
    
  }
  
}

assignment_handler_internal_fabricatr <- function(data, ...) {
  
  options <- quos(...)
  
  fabricate(data = data, !!!options, ID_label = NA)
  
}

assignment_handler_internal_randomizr <-
  function(data, ..., assignment_variable = "Z", append_probabilities_matrix = FALSE) {
    options <- quos(...)
    
    decl <- eval_tidy(quo(declare_ra(N = !!nrow(data), !!!options)), data)    
    
    for (assn in assignment_variable) {
      cond_prob <- as.symbol(paste0(assn, "_cond_prob"))
      assn <- as.symbol(assn)
      if(append_probabilities_matrix) {
        # Creates Z.prob_1 cols
        data <- fabricate(data, !!assn := !!decl$probabilities_matrix, ID_label = NA)
        # change to underscore
        names(data) <- sub(paste0("(?<=",assn,")[.]"), "_", names(data), perl = TRUE)
      }
      
      data <- fabricate(data,
                        !!assn := conduct_ra(!!decl),
                        !!cond_prob := obtain_condition_probabilities(!!decl, assignment = !!assn),
                        ID_label = NA
      )
    }
    
    data
  }

#' @importFrom rlang as_label
validation_fn(assignment_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)
  if(!is.null(eval_tidy(dots[["legacy"]])) && eval_tidy(dots[["legacy"]]) == TRUE) {
    
    dirty <- FALSE
    
    if (!"declaration" %in% names(dots)) {
      if ("blocks" %in% names(dots)) {
        if (inherits(f_rhs(dots[["blocks"]]), "character")) {
          declare_time_error("Must provide the bare (unquoted) block variable name to blocks.", ret)
        }
      }
      
      if ("clusters" %in% names(dots)) {
        if (inherits(f_rhs(dots[["clusters"]]), "character")) {
          declare_time_error("Must provide the bare (unquoted) cluster variable name to clusters.", ret)
        }
      }
      
      ra_args <- setdiff(names(dots), names(formals(assignment_handler_internal_randomizr))) # removes data and assignment_variable
      
      ra_dots <- dots[ra_args]
      
      if (length(ra_dots) > 0) {
        declaration <- tryCatch(eval_tidy(quo(declare_ra(!!!ra_dots))), error = function(e) e)
        
        if (inherits(declaration, "ra_declaration")) {
          # message("Assignment declaration factored out from execution path.")
          dots[ra_args] <- NULL
          dots$declaration <- declaration
          dirty <- TRUE
        }
      }
    }
    
    if ("assignment_variable" %in% names(dots)) {
      if (inherits(f_rhs(dots[["assignment_variable"]]), "NULL")) {
        declare_time_error("Must provide assignment_variable.", ret)
      }
      assignment_variable <- reveal_nse_helper(dots$assignment_variable)
      
      dots$assignment_variable <- assignment_variable
      
      dirty <- TRUE
    } else {
      assignment_variable <- formals(assignment_handler_internal_randomizr)$assignment_variable
    } 
    
    if (dirty) {
      ret <- build_step(currydata(assignment_handler, dots),
                        handler = assignment_handler,
                        dots = dots,
                        label = label,
                        step_type = attr(ret, "step_type"),
                        causal_type = attr(ret, "causal_type"),
                        call = attr(ret, "call")
      )
    }
    
  } else {
    
    randomizr_args <-
      c(
        "blocks",
        "clusters",
        "m",
        "m_unit",
        "m_each",
        "prob",
        "prob_unit",
        "prob_each",
        "block_m",
        "block_m_each",
        "block_prob",
        "block_prob_each",
        "num_arms",
        "conditions",
        "simple"
      )
    
    if("assignment_variable" %in% names(dots)){
      assignment_variable <- get_expr(dots[["assignment_variable"]])
    } else {
      assignment_variable <- "Z"
    }
    
    if(any(randomizr_args %in% names(dots))){
      
      args_quos <- dots[names(dots) %in% randomizr_args]
      
      args_list <- lapply(args_quos, as_label)
      
      suggested_call <-
        paste0(
          "declare_assignment(",
          assignment_variable,
          " = conduct_ra(N = N, ",
          paste0(
            paste0(names(args_list), " = ", args_list),
            collapse = ", "),
          "))")
      
      stop(paste0("You appear to have used legacy declare_assignment() syntax. Consider:\n\n", suggested_call, "\n\nAlternatively, you can set legacy = TRUE to restore the previous functionality.\n\nIf you received this message in error, please ensure that you do not name variables 'blocks', 'clusters', 'm', 'm_unit', 'm_each', 'prob', 'prob_unit', 'prob_each', 'block_m', 'block_m_each', 'block_prob', 'block_prob_each', 'num_arms', 'conditions', or 'simple'."), call. = FALSE)
    } else if (length(names(dots)[!names(dots) %in% "data"]) == 0) {
      stop(paste0("You appear to have used legacy declare_assignment() syntax. Consider:\n\n", "declare_assignment(Z = complete_ra(N = N))", "\n\nAlternatively, you can set legacy = TRUE to restore the previous functionality."), call. = FALSE)
    }
    
  }
  
  structure(ret, step_meta = list(assignment_variables = assignment_variable))
  
}
