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
# takes ... and puts values into appropriate place in design environments
# clones environments before exporting to avoid sharing issues
par_edit <- function(design, ...) {
  updates <- list(...)
  meta <- DeclareDesign:::find_all_objects(design)
  
  steps_to_rebuild <- integer()
  
  for (varname in names(updates)) {
    new_val <- updates[[varname]]
    matches <- meta[meta$name == varname, ]
    if (nrow(matches) == 0) next
    
    for (i in seq_len(nrow(matches))) {
      row <- matches[i, ]
      steps_to_rebuild <- union(steps_to_rebuild, row$step)
      
      old_quosure <- attr(design[[row$step]], "dots")[[row$quosure]]
      old_env <- rlang::get_env(old_quosure)
      
      # Clone and update the quosure's environment
      new_env <- new.env(parent = parent.env(old_env))
      list2env(as.list(old_env, all.names = TRUE), envir = new_env)
      assign(varname, new_val, envir = new_env)
      
      attr(design[[row$step]], "dots")[[row$quosure]] <-
        rlang::new_quosure(rlang::get_expr(old_quosure), env = new_env)
    }
  }
  
  # Rebuild only affected steps using currydata
  for (i in steps_to_rebuild) {
    step <- design[[i]]
    step_attributes <- attributes(step)
    # keeps implicity data data

    # Rename dots and mark as using implicit data
    dots <- DeclareDesign:::rename_dots(step_attributes$handler, step_attributes$dots)
    # attr(dots, "implicit_data_arg") <- TRUE  # 
    
    new_step <- DeclareDesign:::currydata(step_attributes$handler, dots)
    attributes(new_step) <- step_attributes
    design[[i]] <- new_step
  }
  
  design
}

