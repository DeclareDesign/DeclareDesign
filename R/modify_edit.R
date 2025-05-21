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
clone_design_edit_old <- function(design, ..., to_replace = list(...)) {
  design[] <- lapply(design, clone_step_edit, to_replace = to_replace)

  design
}

# takes ... and puts values into appropriate place in design environments
# clones environments before exporting to avoid sharing issues
clone_design_edit <- function(design, ...) {
  
  dots <- rlang::list2(...)
  
  # Clone first
  cloned_design <- clone_design_envs(design)
  
  # Update environments
  all_objs <- find_all_objects(cloned_design)
  
  for (name in names(dots)) {
    matches <- dplyr::filter(all_objs, .data$name == !!name)
    
    if (nrow(matches) == 0) {
      warning(glue::glue("No match found for '{name}' in design environments"))
      next
    }
    
    for (i in seq_len(nrow(matches))) {
      env <- matches$env[[i]]
      assign(name, dots[[name]], envir = env)
    }
  }
  
  # Now re-evaluate steps that have modified environments
  modified_steps <- unique(dplyr::filter(all_objs, name %in% names(dots))$step)
  
  for (step_i in modified_steps) {
    step <- cloned_design[[step_i]]
    
    # Retrieve and re-evaluate the step using its constructor
    step_type <- attr(step, "step_type")
    dots <- attr(step, "dots")
    call <- attr(step, "call")
    
    # Re-evaluate call in the correct environment
    eval_env <- environment(call)
    
    # Reconstruct the step
    rebuilt <- eval(call, envir = eval_env)
    
    # Preserve attributes that are not overwritten
    attributes(rebuilt)$call <- call
    attributes(rebuilt)$step_type <- step_type
    attributes(rebuilt)$dots <- dots
    
    cloned_design[[step_i]] <- rebuilt
  }
  
  cloned_design
}
