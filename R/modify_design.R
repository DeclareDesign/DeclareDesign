# index of a step (specified by object, label or position)
find_step <- function(design, step, verb) {
  if (is.numeric(step) && step <= length(design) && step > 0) return(step)
  if (is.character(step)) {
    design <- names(design)
  }
  w <- vapply(design, identical, FALSE, step)

  w <- which(w)
  if (length(w) == 0) {
    stop("Could not find step to ", verb, " in design")
  }

  w[1]
}


#' Modify a design after the fact
#'
#' Insert, delete and replace steps in an (already declared) design object.
#'
#' @param design A design object, usually created using the + operator, \code{\link{expand_design}}, or the design library.
#'
#' @return A new design object.
#'
#' @importFrom rlang call_name quos quo_expr quo_is_missing
#' @name modify_design
#' @rdname modify_design
#'
#' @examples
#'
#'  my_model <- 
#'    declare_model(
#'      N = 100, 
#'      U = rnorm(N),
#'      Y_Z_0 = U,
#'      Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
#'    )
#'
#'  my_assignment <- declare_assignment(Z = complete_ra(N, m = 50))
#'  my_assignment_2 <- declare_assignment(Z = complete_ra(N, m = 25))
#'
#'  design <- my_model + my_assignment
#'
#'  draw_data(design)
#'  
#'  design_modified <- replace_step(design, 2, my_assignment_2)
#'  
#'  draw_data(design)
NULL

#' @param before The step before which to add steps.
#' @param after The step after which to add steps.
#' @param new_step The new step; Either a function or a partial call.
#'
#' @details See \code{\link{modify_design}} for details.
#' @rdname modify_design
#' @examples
#'  
#'  \dontrun{
#'  
#'  design <- 
#'    declare_model(
#'      N = 100,
#'      U = rnorm(N),
#'      potential_outcomes(Y ~ 0.20 * Z + U)
#'    ) + 
#'      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
#'      declare_assignment(Z = complete_ra(N, m = N/2)) + 
#'      declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
#'      declare_estimator(Y ~ Z, inquiry = "ATE")
#'  
#'  insert_step(design, declare_sampling(S = complete_rs(N, n = 50)),
#'              after = 1)
#'
#'  # If you are using a design created by a designer, for example from
#'  #   the DesignLibrary package, you will not have access to the step
#'  #   objects. Instead, you can always use the label of the step.
#'  
#'  design <- DesignLibrary::two_arm_designer()
#'  
#'  # get the labels for the steps
#'  names(design)
#'  
#'  insert_step(design, 
#'    declare_sampling(S = complete_rs(N, n = 50)),
#'    after = "potential_outcomes")
#'    
#'  }
#'
#' @export
insert_step <- function(design, new_step, before, after) {
  check_design_class_single(design)
  
  if (missing(before)) before <- NULL
  if (missing(after)) after <- NULL
  insert_step_(design, new_step, before, after, enexpr(new_step))
}

insert_step_ <- function(design, new_step, before = NULL, after = NULL, new_step_expr) {
  check_design_class_single(design)
  
  if (is.null(after)) {
    if (is.null(before)) {
      stop("Must provide either before or after to add_step()")
    }
    after <- find_step(design, before, "insert before") - 1
  } else {
    after <- find_step(design, after, "insert after")
  }

  new_step <- wrap_step(new_step, new_step_expr)

  i <- seq_along(design) <= after
  steps <- c(design[i], new_step, design[!i], recursive = FALSE)

  construct_design(steps)
}

#' @param step The quoted label of the step to be deleted or replaced.
#'
#' @export
#' @rdname modify_design
#' @examples
#' 
#' design <- 
#'   declare_model(
#'     N = 100,
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ 0.20 * Z + U)
#'   ) + 
#'     declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
#'     declare_assignment(Z = complete_ra(N, m = N/2)) + 
#'     declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
#'     declare_estimator(Y ~ Z, inquiry = "ATE")
#
#' delete_step(design, step = 5)
#' 
delete_step <- function(design, step) {
  check_design_class_single(design)
  
  i <- find_step(design, step, "delete")
  construct_design(design[-i])
}

#' @export
#' @rdname modify_design
#' @examples
#' 
#' design <- 
#'   declare_model(
#'     N = 100,
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ 0.20 * Z + U)
#'   ) + 
#'     declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
#'     declare_assignment(Z = complete_ra(N, m = N/2)) + 
#'     declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
#'     declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' replace_step(
#'   design, 
#'   step = 3, 
#'   new_step = declare_assignment(Z = simple_ra(N, prob = 0.5)))
replace_step <- function(design, step, new_step) {
  check_design_class_single(design)
  
  i <- find_step(design, step, "replace")
  new_step <- wrap_step(new_step, enexpr(new_step))
  design[i] <- new_step
  names(design)[i] <- names(new_step)

  construct_design(design)
}
