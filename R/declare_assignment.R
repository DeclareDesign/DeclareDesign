#' Declare Data Strategy: Assignment
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return A function that takes a data.frame as an argument and returns a data.frame with assignment columns appended.
#' @export
#'
#' @examples
#'
#' # setting up a design stub
#' design <- declare_model(
#'   classrooms = add_level(10),
#'   individuals = add_level(20, female = rbinom(N, 1, 0.5))
#' ) + NULL
#' 
#' # Declare assignment of m units to treatment
#' design + declare_assignment(Z = complete_ra(N = N, m = 100))
#' 
#' # Declare assignment specifying varying block probabilities
#' design + 
#'   declare_assignment(Z = block_ra(blocks = female, 
#'                                   block_prob = c(1/3, 2/3)))
#' 
#' # Declare assignment of clusters with probability 1/4
#' design + declare_assignment(
#'   Z = cluster_ra(prob = 1/4, clusters = classrooms))
#'  
#' # Declare factorial assignment (Approach 1): 
#' #   Use complete random assignment to assign Z1 
#' #   then block on Z1 to assign Z2. 
#' design + 
#'    declare_assignment(Z1 = complete_ra(N = N, m = 100),
#'                       Z2 = block_ra(blocks = Z1))
#'    
#' # Declare factorial assignment (Approach 2): 
#' #   Assign to four conditions and then split into Z1 and Z2 
#' design +  
#'   declare_assignment(Z = complete_ra(N = N, conditions = 1:4),
#'                      Z1 = as.numeric(Z %in% 2:3), 
#'                      Z2 = as.numeric(Z %in% 3:4))
#'    
#' # Declare assignment using functions outside randomizr package:
#' design + 
#'   declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))
#' 
declare_assignment <- make_declarations(assignment_handler, "assignment")

#' @param data A data.frame.
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_assignment
assignment_handler <- function(data, ...) {
  
  options <- quos(...)
  
  fabricate(data = data, !!!options, ID_label = NA)
  
}

#' @importFrom rlang quos !!! call_modify eval_tidy quo f_rhs
#' @importFrom randomizr conduct_ra obtain_condition_probabilities declare_ra
#' @param assignment_variable Name for assignment variable (quoted). Defaults to "Z". Argument to be used with default handler. 
#' @param append_probabilities_matrix Should the condition probabilities matrix be appended to the data? Defaults to FALSE.  Argument to be used with default handler.
#' @param data A data.frame.
#' @rdname declare_assignment
assignment_handler_legacy <-
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

validation_fn(assignment_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  dirty <- FALSE

  if (!"declaration" %in% names(dots)) {
    if ("blocks" %in% names(dots)) {
      if (class(f_rhs(dots[["blocks"]])) == "character") {
        declare_time_error("Must provide the bare (unquoted) block variable name to blocks.", ret)
      }
    }

    if ("clusters" %in% names(dots)) {
      if (class(f_rhs(dots[["clusters"]])) == "character") {
        declare_time_error("Must provide the bare (unquoted) cluster variable name to clusters.", ret)
      }
    }

    ra_args <- setdiff(names(dots), names(formals(assignment_handler))) # removes data and assignment_variable

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
    if (class(f_rhs(dots[["assignment_variable"]])) == "NULL") {
      declare_time_error("Must provide assignment_variable.", ret)
    }
    assn <- reveal_nse_helper(dots$assignment_variable)

    dots$assignment_variable <- assn

    dirty <- TRUE
  } else {
    assn <- formals(assignment_handler)$assignment_variable
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

  structure(ret, step_meta = list(assignment_variables = assn))
}
