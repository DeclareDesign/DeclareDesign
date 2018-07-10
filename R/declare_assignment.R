#' Declare assignment procedure
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame with additional columns appended including an assignment variable and (optionally) probabilities of assignment.
#' @export
#'
#' @details
#'
#' While declare_assignment can work with any assignment_function that takes data and returns data, most random assignment procedures can be easily implemented with randomizr.
#' The arguments to \code{\link{conduct_ra}} can include N, block_var, clust_var, m, m_each, prob, prob_each, block_m, block_m_each = NULL, block_prob, block_prob_each, num_arms, and conditions.
#' The arguments you need to specify are different for different designs. Check the help files for \code{\link{complete_ra}}, \code{\link{block_ra}}, \code{\link{cluster_ra}}, or \code{\link{block_and_cluster_ra}} for details on how to execute many common designs.
#'
#' Custom assignment handlers should augment the data frame with an appropriate column for the assignments.
#'
#' @importFrom rlang quos quo lang_modify eval_tidy !!!
#' @importFrom randomizr declare_ra
#'
#' @examples
#'
#' # Default Handler
#' # Delegates to conduct_ra
#'
#' my_assignment <- declare_assignment(m = 50)
#' my_assignment <- declare_assignment(block_prob = 1/3, blocks = female)
#' my_assignment <- declare_assignment(block_prob = 1/4, clusters = classrooms)
#'
#' my_assignment <- declare_assignment(
#'   block_prob = 1/4,
#'   clusters = classrooms,
#'   assignment_variable = "X1"
#' )
#'
#' # Custom random assignment functions
#'
#' my_assignment_function <- function(data) {
#'    data$Z <- ifelse(data$extra <= median(data$extra), 1, 0)
#'    data
#' }
#'
#' my_assignment_custom <- declare_assignment(handler = my_assignment_function)
#'
#' df <- my_assignment_custom(sleep)
#' table(df$Z, df$group)
#' 
declare_assignment <- make_declarations(assignment_handler, "assignment")


#' @importFrom rlang quos !!! lang_modify eval_tidy quo f_rhs
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
#' @param assignment_variable name for assignment variable
#' @param append_probabilities_matrix Should the condition probabilities matrix be appended to the data? Defaults to FALSE
#' @param data a data.frame
#' @rdname declare_assignment
assignment_handler <-
  function(data, ..., assignment_variable = "Z", append_probabilities_matrix = FALSE) {
    
    options <- quos(...)
    
    for (assn in assignment_variable) {
      cond_prob <- as.symbol(paste0(assn, "_cond_prob"))
      assn <- as.symbol(assn)
      data <- fabricate(data,
                        !!assn      := conduct_ra(N = N,!!!options),
                        !!cond_prob := obtain_condition_probabilities(!!!options, assignment = !!assn),
                        ID_label = NA
      )
      if (append_probabilities_matrix) {
        options$N <- quo(nrow(data))
        ra_dec <- eval_tidy(quo(declare_ra(!!!options)))
        probabilities_matrix <- ra_dec$probabilities_matrix
        colnames(probabilities_matrix) <- paste0(assn, "_", colnames(probabilities_matrix))
        data <- data.frame(data, probabilities_matrix)
      }
    }

    data
  }

validation_fn(assignment_handler) <- function(ret, dots, label){

  declare_time_error_if_data(ret)
  
  declare_time_error_label_length(ret, label)

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

    ra_args <- setdiff(names(dots), names(formals(assignment_handler))) #removes data and assignment_variable

    ra_dots <- dots[ra_args]

    if (length(ra_dots) > 0) {
      declaration <- tryCatch(eval_tidy(quo(declare_ra(!!!ra_dots))), error = function(e)e)

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
    ret <- build_step(currydata(assignment_handler, dots, strictDataParam = attr(ret, "strictDataParam")),
                      handler = assignment_handler,
                      dots = dots,
                      label = label,
                      step_type = attr(ret, "step_type"),
                      causal_type = attr(ret, "causal_type"),
                      call = attr(ret, "call"))
  }

  structure(ret, step_meta = list(assignment_variables = assn))
  
}
