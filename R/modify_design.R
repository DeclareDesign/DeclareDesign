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
#' @param design a design object, usually created using the + operator, \code{\link{expand_design}}, or the design library.
#'
#' @return A new design object
#'
#' @importFrom rlang lang_name quos quo_expr quo_is_missing
#' @name modify_design
#' @rdname modify_design
#'
#' @examples
#'
#'  my_population <- declare_population(N = 100, noise = rnorm(N))
#'
#'  my_potential_outcomes <-
#'    declare_potential_outcomes(Y_Z_0 = noise,
#'                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
#'
#'  my_assignment <- declare_assignment(m = 50)
#'  my_assignment_2 <- declare_assignment(m = 25)
#'
#'  design <- my_population + my_potential_outcomes + my_assignment
#'
#'  design
#'
NULL

#' @param before the step before which to add steps
#' @param after same
#' @param new_step the new step - either a function or a partial call
#'
#' @details see \code{\link{modify_design}} for details.
#' @rdname modify_design
#' @examples
#'
#'  insert_step(design, declare_step(dplyr::mutate, income = noise^2), after = my_assignment)
#'  insert_step(design, declare_step(dplyr::mutate, income = noise^2), before = my_assignment)
#'
#' @export
insert_step <- function(design, new_step, before, after) {
  if(missing(before)) before <- NULL
  if(missing(after)) after <- NULL
  insert_step_(design, new_step, before, after, enexpr(new_step))
}

insert_step_ <- function(design, new_step_quosure, before = NULL, after = NULL, new_step_expr) {
  if (is.null(after)) {
    if (is.null(before)) {
      stop("Must provide either before or after to add_step()")
    }
    after <- find_step(design, before, "insert after") - 1
  } else {
    after <- find_step(design, after, "insert befor")
  }
  
  new_step <- wrap_step(new_step, new_step_expr)
  
  i <- seq_along(design) <= after
  steps <- c(design[i], new_step, design[!i], recursive = FALSE)
  
  construct_design(steps)
  
}

#' @param step the step to be deleted or replaced
#'
#' @export
#' @rdname modify_design
#' @examples
#'
#'  delete_step(design, my_assignment)
delete_step <- function(design, step) {
  i <- find_step(design, step, "delete")
  construct_design(design[-i])
}

#' @export
#' @rdname modify_design
#' @examples
#'  replace_step(design, my_assignment, declare_step(dplyr::mutate, words = "income"))
replace_step <- function(design, step, new_step) {
  i <- find_step(design, step, "replace")
  new_step <- wrap_step(step, enexpr(step))
  design[i] <- new_step
  names(design)[i] <- names(new_step)
  
  construct_design(design)
}
