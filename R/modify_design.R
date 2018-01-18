find_step <- function(design, step) {
  if(is.numeric(step) && step <= length(design) && step > 0) return(step)
  if(is.character(step)) {
    design <- lapply(design, attr, "label")
  }
  w <- vapply(design, identical, FALSE, step)

  w <- which(w)[1]
  if(!is.integer(w)){
    stop("Could not find step (", substitute(step), ") in design")
  }

  w
}


#' Modify a design after the fact
#'
#' Insert, delete and replace steps in an (already declared) design object.
#'
#' @param design a design object, usually created by \code{\link{declare_design}}, \code{\link{quick_design}}, or \code{\link{download_design}}.
#'
#'
#' @return A new design object. See \code{\link{declare_design}} for details.
#'
#' @importFrom rlang lang_name quos quo_expr
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
#'  design <- declare_design(my_population,
#'                           my_potential_outcomes,
#'                           my_assignment)
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
#'  insert_step(design, dplyr::mutate(income = noise^2), after = my_assignment)
#'  insert_step(design, dplyr::mutate(income = noise^2), before = my_assignment)
#'
#' @export
insert_step <- function(design, new_step, before = NULL, after = NULL) {
  insert_step_(design, enquo(new_step), before, after)
}

insert_step_ <- function(design, new_step_quosure, before = NULL, after = NULL) {
  if(is.null(after)) {
    if(is.null(before)) {
      stop("Must provide either before or after to add_step()")
    }
    after <- find_step(design, before) - 1
  } else {
    after <- find_step(design, after)
  }

  new_step <- tryCatch(
    eval_tidy(new_step_quosure),
    error = function(e) callquos_to_step(new_step_quosure)
  )


  i <- seq_along(design)
  structure(
    c(design[i <= after], list(new_step), design[i > after], recursive=FALSE),
    class = "design"
  )
}

#' @param step the step to be deleted or replaced
#'
#' @export
#' @rdname modify_design
#' @examples
#'
#'  delete_step(design, my_assignment)
delete_step <- function(design, step) {
  i <- find_step(design, step)
  structure(design[-i], class = "design")
}

#' @export
#' @rdname modify_design
#' @examples
#'  replace_step(design, my_assignment, dplyr::mutate(words="HIARYLAH"))
replace_step <- function(design, step, new_step) {
  delete_step(
    insert_step_(design, after = step, new_step_quosure = enquo(new_step)),
    step)
}
