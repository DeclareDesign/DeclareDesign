#' Find all objects and variables used in a design or design step
#'
#' @description This internal function analyzes a DeclareDesign object to find all variables,
#' functions, and objects that are saved within the design steps. It identifies objects
#' from quosures, handler environments, and other design components to provide a comprehensive
#' view of objects the design depends on.
#'
#' This function is primarily used internally for design analysis and debugging purposes.
#' It helps identify dependencies and understand what objects a design relies on.
#'
#' @param design A design object or design step created using DeclareDesign functions
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{name}{The name of the object/variable}
#'   \item{value_str}{String representation of the object's value or type}
#'   \item{step}{The step number where the object was found}
#'   \item{quosure}{The name of the quosure or "handler" where the object was found}
#'   \item{env}{The environment object where the variable was found}
#' }
#'
#' @keywords internal
find_all_objects <- function(design) {

  if (!any(c("design_step", "design") %in% class(design))) 
    stop("Please pass a design object or design step")   
  
  if ("design_step" %in% class(design)) 
    design <- design + NULL
  
  results <- list()
  
  # Step by step
  for (step_i in seq_along(design)) {
    step <- design[[step_i]]
    
    jobs <- list()
    handler_jobs <- list()
    handler_arg_jobs <- list()
    
    # --- Extract quosures from dots
    dots <- attr(step, "dots")
    if (!is.null(dots)) {
      if ((length(names(dots)[names(dots) == ""]) > 1) & (attributes(step)$step_type != "model"))
        stop(paste("More than one unnamed quosure in step", step_i))
      
      names(dots)[names(dots) == ""] <- "formula"
      
      for (quosure_name in names(dots)) {
        q <- dots[[quosure_name]]
        if (rlang::is_quosure(q)) {
          q <- rlang::as_quosure(q)
          env <- rlang::get_env(q)
          jobs[[length(jobs) + 1]] <- list(
            name = quosure_name,
            env = env,
            step = step_i
          )
        }
      }
    }
    
    
    # --- Add handler environment
    handler <- attr(step, "handler")

    if (!is.null(handler)) {
      handler_env <- rlang::get_env(handler)
      

      # skip known internal handlers or package-defined handlers
      # skip estimator steps since these are wrapped and messy
      if (
        (attr(step, "step_type") != "estimator") && 
        !(attr(handler, "tag") %in% c("fabricate", "potential_outcomes_handler", "assignment_handler")) &&
        !isNamespace(handler_env) &&
        !startsWith(environmentName(handler_env), "namespace:")
      ) {
        handler_name <- if (!is.null(attr(handler, "tag"))) attr(handler, "tag") else "handler"
        handler_jobs[[length(handler_jobs) + 1]] <- 
          list(
          name = handler_name,
          env = handler_env,
          step = step_i
        )
        for(arg in  ls(handler_env)) {
          handler_arg_jobs[[length(handler_arg_jobs) + 1]] <- 
            list(
              name = arg,
              env = handler_env,
              step = step_i
            )
        }
        
      }
    }
    
    # --- Process all jobs (quosures + handler)
    #########################################
    for (job in jobs) {
      for (name in ls(job$env, all.names = TRUE)) {
        
        # hide currydata internals
        if (name %in% c("dots", "quoData", "quoNoData")) next
        
        val <- tryCatch(get(name, envir = job$env), error = function(e) "<error>")
        val_str <- tryCatch({
          if (is.atomic(val) && length(val) <= 5) {
            paste0(deparse(val), collapse = "")
          } else if (is.function(val)) {
            "function"
          } else {
            paste0("<", class(val)[1], ">")
          }
        }, error = function(e) "<error>")
      

        results[[length(results) + 1]] <- data.frame(
          name = name,
          value_str = val_str,
          step = job$step,
          quosure = job$name,
          env = I(list(job$env)),
          stringsAsFactors = FALSE
        )

      }
    }
    # handler arg  jobs
    #########################################
    for (job in handler_arg_jobs) {

      val <- tryCatch(get(job$name, envir = job$env), error = function(e) "<error>")
      val_str <- tryCatch({
        if (is.atomic(val) && length(val) <= 5) {
          paste0(deparse(val), collapse = "")
        } else if (is.function(val)) {
          "function"
        } else {
          paste0("<", class(val)[1], ">")
        }
      }, error = function(e) "<error>")

            
      results[[length(results) + 1]] <- data.frame(
        name = job$name,
        value_str = val_str,
        step = job$step,
        quosure = "handler",
        env = I(list(job$env)),
        stringsAsFactors = FALSE
      )
      
    }    
    # should be only one handler per step
    for (job in handler_jobs) {

        results[[length(results) + 1]] <- data.frame(
          name = job$name,
          value_str = "handler",
          step = job$step,
          quosure = job$name,
          env = I(list(job$env)),
          stringsAsFactors = FALSE
        )
        
    }
    
  }
  
  
  x <- do.call(rbind, results)     
  
  if(is.null(x)) 
    x <- data.frame()

  row.names(x) <- NULL
  #  env_label <- vapply(lapply(results, `[[`, "env"),
  #                      function(z) rlang::env_label(z[[1]]), character(1))
  #  x$env_label <- env_label 
  
  class(x) <- c("objects", class(x))

  x
}


#' @keywords internal
#' @exportS3Method print objects
print.objects <- function(x, ...) {
  
  if(nrow(x) ==0) {
    print("No parameters")
    return(NULL)
  }
  
  tmp <- x[c("name", "value_str", "step")] |> unique()
  
  # aggregate steps per (name, value_str)
  out <- aggregate(
    step ~ name + value_str,
    data = tmp,
    FUN = function(s) paste(sort(unique(s)), collapse = ", ")
  )
  
  # rename aggregated column
  names(out)[names(out) == "step"] <- "steps"
  
  # ensure plain data.frame (no tibble class if it ever sneaks in)
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  # sort by name (case insensitive)
  out <- out[order(tolower(out$name), na.last = TRUE), , drop = FALSE]
  
  # print and return invisibly
  print(out, row.names = FALSE)
  invisible(out)
  
}


