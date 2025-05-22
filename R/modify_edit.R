#' Modify internal variables in a design
#'
#' Internal helper for `redesign()`. Updates variable values inside quosure environments
#' of a design object so that rerunning the design reflects new inputs.
#'
#' @param design A design object created by DeclareDesign.
#' @param ... Named variable updates (e.g., `N = 20`, `b = 0.5`).
#'
#' @return A design with updated variables.
#' @keywords internal
#'
#' @examples
#' d <- declare_model(N = n, Y = rnorm(N, b)) + declare_inquiry(Q = b)
#' d2 <- redesign(d, n = 3, b = 0.2)
#' draw_data(d2)

modify_edit <- function(design, ...) {
  
  updates <- list(...)
  meta <- DeclareDesign:::find_all_objects(design)
  
  steps_to_rebuild <- integer()
  
  # Going through each parameter
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

    step_attributes$dots[] <- 
      DeclareDesign:::rename_dots(step_attributes$handler, step_attributes$dots)
    new_step <- with(step_attributes, currydata(handler, dots))

    attributes(new_step) <- step_attributes

    design[[i]] <- new_step
    
  }
  
  design
}





x_modify_edit <- function(design, ...) {
  updates <- list(...)
  meta <- DeclareDesign:::find_all_objects(design)
  
  # Map: step number -> list( quosure index -> list of varnames to update )
  step_quosure_updates <- list()
  
  # Group updates per quosure
  for (varname in names(updates)) {
    matches <- meta[meta$name == varname, ]
    if (nrow(matches) == 0) next
    
    for (i in seq_len(nrow(matches))) {
      step_i <- matches$step[i]
      quo_i <- matches$quosure[i]
      
      if (is.null(step_quosure_updates[[as.character(step_i)]])) {
        step_quosure_updates[[as.character(step_i)]] <- list()
      }
      
      step_entry <- step_quosure_updates[[as.character(step_i)]]
      if (is.null(step_entry[[quo_i]])) {
        step_entry[[quo_i]] <- character()
      }
      
      step_entry[[quo_i]] <- union(step_entry[[quo_i]], varname)
      step_quosure_updates[[as.character(step_i)]] <- step_entry
    }
  }
  
  # Apply updates once per quosure
  for (step_str in names(step_quosure_updates)) {
    i <- as.integer(step_str)
    step <- design[[i]]
    step_attributes <- attributes(step)
    
    for (quo_i in names(step_quosure_updates[[step_str]])) {
      quo_idx <- as.integer(quo_i)
      varnames <- step_quosure_updates[[step_str]][[quo_i]]
      
      old_quosure <- attr(step, "dots")[[quo_idx]]
      old_env <- rlang::get_env(old_quosure)
      
      # Clone and modify env only once
      new_env <- new.env(parent = parent.env(old_env))
      list2env(as.list(old_env, all.names = TRUE), envir = new_env)
      for (varname in varnames) {
        assign(varname, updates[[varname]], envir = new_env)
      }
      
      attr(step, "dots")[[quo_idx]] <- rlang::new_quosure(
        rlang::get_expr(old_quosure), env = new_env
      )
    }
    
    # Rebuild the step with new dots
    dots <- DeclareDesign:::rename_dots(step_attributes$handler, attr(step, "dots"))
    new_step <- DeclareDesign:::currydata(step_attributes$handler, dots)
    
    # Only reattach safe attributes
    attributes(new_step)$label <- step_attributes$label
    attributes(new_step)$step_type <- step_attributes$step_type
    attributes(new_step)$handler <- step_attributes$handler
    attributes(new_step)$dots <- dots
    
    design[[i]] <- new_step
  }
  
  design
}


