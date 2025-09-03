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
      
      
      # Treat parameters in quosures in dots and parameters in handlers separately
      par_in_handler = row$quosure == "handler"
      
      if (par_in_handler) {
        h <- attr(design[[row$step]], "handler")
        old_env <- environment(h)
        
        # Clone and update the handler's environment
        new_env <- new.env(parent = parent.env(old_env))
        list2env(as.list(old_env, all.names = TRUE), envir = new_env)
        assign(varname, new_val, envir = new_env)
        
        # Clone the function and set new environment
        body_expr <- body(h)
        formals_expr <- formals(h)
        new_handler <- eval(call("function", formals_expr, body_expr), envir = new_env)
        
        attr(design[[row$step]], "handler") <- new_handler
      }      

      if(!par_in_handler) {
        row_dots <-   attr(design[[row$step]], "dots")
      
        # Figure the position of the quosure to change; whether names or not
        # This is because some formula quosures might not be named
        missing_formula_names <-
                (row$quosure == "formula") & (!("formula" %in% names(row_dots)))
  
        positn <- ifelse(missing_formula_names,
                         which(names(row_dots) == "")[1],
                         which(names(row_dots) == row$quosure)[1])
  
        old_quosure <- row_dots[[positn]] 
      
        old_env <- rlang::get_env(old_quosure)
        
        # Clone and update the quosure's environment
        new_env <- new.env(parent = parent.env(old_env))
        list2env(as.list(old_env, all.names = TRUE), envir = new_env)
        assign(varname, new_val, envir = new_env)
        
        attr(design[[row$step]], "dots")[[positn]] <-
          rlang::new_quosure(rlang::get_expr(old_quosure), env = new_env)
        
      }
      
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




