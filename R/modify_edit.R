#' Override environment via shim
#'
#' @rdname edit
#' @keywords internal
#' @examples
#' \dontrun{
#' here_i_am <- "foo"
#' dot <- quo(here_i_am)
#' dot2 <- DeclareDesign:::clone_dot_edit_env(dot, here_i_am = "some_message", xyxyx = "bar")
#' rlang::eval_tidy(dot)
#' rlang::eval_tidy(dot2)
#' }
clone_dot_edit_env <- function(dot, ..., to_replace = list(...)) {
  if (is.null(environment(dot))) {
    return(dot)
  }
  environment(dot) <- list2env(to_replace, parent = environment(dot))

  dot
}

#' @rdname edit
#' @keywords internal
#' @examples
#' \dontrun{
#' N <- 50
#'
#' pop50 <- declare_model(N=N, noise=rnorm(N))
#' nrow(pop50())
#'
#' pop100 <- DeclareDesign:::clone_step_edit(pop50, N=100)
#' nrow(pop100())
#' nrow(pop50())
#'
#' }
clone_step_edit <- function(step, ..., to_replace = list(...)) {
  step_attributes <- attributes(step)

  step_attributes$dots[] <- lapply(step_attributes$dots, clone_dot_edit_env, to_replace = to_replace)

  f <- with(step_attributes, currydata(handler, dots))
  attributes(f) <- step_attributes
  f
}

#' @rdname edit
#' @keywords internal
clone_design_edit <- function(design, ..., to_replace = list(...)) {
  design[] <- lapply(design, clone_step_edit, to_replace = to_replace)

  design
}
