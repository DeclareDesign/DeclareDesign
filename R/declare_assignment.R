

#' Declare Assignment Procedure
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that takes a data.frame as an argument and returns a data.frame with additional columns appended including an assignment variable and (optionally) probabilities of assignment.
#' @export
#'
#' @details
#'
#' While declare_assignment can work with any assignment_function that takes data and returns data, most random assignment procedures can be easily implemented with randomizr. The arguments to \code{\link{conduct_ra}} can include N, block_var, clust_var, m, m_each, prob, prob_each, block_m, block_m_each = NULL, block_prob, block_prob_each, num_arms, and conditions. The arguments you need to specify are different for different designs. Check the help files for \code{\link{complete_ra}}, \code{\link{block_ra}}, \code{\link{cluster_ra}}, or \code{\link{block_and_cluster_ra}} for details on how to execute many common designs.
#'
#' @importFrom rlang quos quo lang_modify eval_tidy !!!
#' @importFrom randomizr declare_ra
#'
#' @examples
#'
#' my_population <- declare_population(N = 100, female = rbinom(N, 1, .5))
#' df <- my_population()
#'
#' # Complete random assignment using randomizr
#' # use any arguments you would use in conduct_ra.
#'
#' my_assignment <- declare_assignment(m = 50)
#' df <- my_assignment(df)
#' head(df)
#' table(df$Z)
#'
#' # Block random assignment
#'
#' my_blocked_assignment <- declare_assignment(blocks = female)
#'
#' df <- my_population()
#'
#' df <- my_blocked_assignment(df)
#' head(df)
#' with(df, table(Z, female))
#'
#'
#' # Custom random assignment functions
#'
#' df <- my_population()
#'
#' my_assignment_function <- function(data) {
#'    data$Z <- rbinom(n = nrow(data),
#'    size = 1,
#'    prob = 0.5)
#'    data
#'    }
#'
#' my_assignment_custom <- declare_assignment(
#'    handler = my_assignment_function)
#'
#' df <- my_assignment_custom(df)
#' head(df)
#' table(df$Z)
declare_assignment <- make_declarations(assignment_handler, "assignment" )


#' @importFrom rlang quos !!! lang_modify eval_tidy quo f_rhs
#' @importFrom randomizr conduct_ra obtain_condition_probabilities
assignment_handler <-
  function(data, ..., assignment_variable = "Z") {
    ## draw assignment

    options <- quos(...)


    for(assn in assignment_variable){
      cond_prob <- as.symbol(paste0(assn, "_cond_prob"))
      assn <- as.symbol(assn)
      data <- fabricate(data,
                        !!assn      := conduct_ra(N=N, !!!options),
                        !!cond_prob := obtain_condition_probabilities(!!!options, assignment = !!assn),
       ID_label = NA
      )
    }


    data
  }

validation_fn(assignment_handler) <-   function(ret, dots, label){

  declare_time_error_if_data(ret)

  dirty <- FALSE
  # browser()

  if(! "declaration" %in% names(dots)) {
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

    if(length(ra_dots) > 0) {
      declaration <- tryCatch(eval_tidy(quo(declare_ra(!!!ra_dots))), error=function(e)e)

      if(inherits(declaration, "ra_declaration")) {
        message("Assignment declaration factored out from execution path.")
        dots[ra_args] <- NULL
        dots$declaration <- declaration
        dirty <- TRUE
      }
    }


  }

  if("assignment_variable" %in% names(dots)){
    if (class(f_rhs(dots[["assignment_variable"]])) == "NULL") {
      declare_time_error("Must provide assignment_variable.", ret)
    }
    assn <- reveal_nse_helper(dots$assignment_variable)

    dots$assignment_variable <- assn

    dirty <- TRUE

  }
  else {
    assn <- formals(assignment_handler)$assignment_variable
  }

  if(dirty) {
    ret <- build_step(currydata(assignment_handler, dots, strictDataParam=attr(ret, "strictDataParam")),
                      handler=assignment_handler,
                      dots=dots,
                      label=label,
                      step_type=attr(ret, "step_type"),
                      causal_type=attr(ret,"causal_type"),
                      call=attr(ret, "call"))
  }



  structure(ret, step_meta=list(assignment_variables=assn))
}

###############################################################################


