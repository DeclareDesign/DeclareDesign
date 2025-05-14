
# Given a function and dots, rename dots based on how things will positionally match
#' @importFrom rlang is_empty is_scalar_character get_expr
rename_dots <- function(handler, dots) {
  
  initial_call <- match.call(handler, as.call(get_expr(quo(handler(!!!dots)))))
  
  free_vars <- setdiff(names(formals(handler)), names(initial_call))
  
  if(length(free_vars) > 0 & !any(c('data', '.data') %in% names(dots))) {
    # first check if ^data argument was already provided by user in dots.
    # DD data param in this comment
    dot_is_data <- vapply(lapply(dots, get_expr), identical, TRUE, quote(data))
    
    if(!any(dot_is_data)) {
    
      # To make handlers quasi-compatible with hadley naming of functions
      # eg .data and not data
      
      # If there is neither a data or .data, match positional argument #1
      data_arg <- list(quote(data))
      data_arg_name <- names(formals(handler)) %i% c('data', '.data')
      if(is_scalar_character(data_arg_name)) {
        names(data_arg) <- data_arg_name
      }
      
      dots <- append(dots, data_arg, after = FALSE)
      is_implicit_data_arg(dots) <- TRUE
    }
  }   
  
  if (is_empty(dots)) {
    return(dots)
  }
  
  f <- function(...) as.list(match.call(handler)[-1])
  
  # Match positions to parameter names of handler, copy names onto dots.
  # Defensive, will help when handlers are wrapped in HOF
  d_idx <- setNames(seq_along(dots), names(dots))
  d_idx <- eval_tidy(quo(f(!!!d_idx)))
  d_idx <- unlist(d_idx)
  d_idx <- d_idx[names(d_idx) != "" ]

  names(dots)[d_idx] <- names(d_idx)

  dots
}

# Returns a new function(data) which calls FUN(data, dots)
currydata <- function(FUN, dots) {

  quoData <- quo((FUN)(!!!dots))
  quoNoData <- quo((FUN)(!!!(dots[names(dots) != 'data'])))
  
  function(data = NULL) {
    #message(quo)
    # used for declare_model with no seed data provided, in which case null is not the same as missing.
    # Unfortunately, steps do not know at time of declaration if they are in first position or not; 
    # combining steps into design happens after.
    # This could in theory be caught be a design validation function for declare_model.

    eval_tidy(if (is.null(data) & is_implicit_data_arg(dots)) quoNoData else quoData, data = list(data = data))
  }
}



# Helper to find all symbols recursively in an expression
find_symbols_recursive <- function(expr) {
  if (is.null(expr)) return(character())
  symbols <- character()
  if (is.symbol(expr)) {
    symbols <- as.character(expr)
  } else if (is.call(expr) || is.pairlist(expr)) {
    for (e in as.list(expr)) {
      symbols <- c(symbols, find_symbols_recursive(e))
    }
  }
  unique(symbols)
}

# Helper to capture globals for functions, recursively
is_available_from_loaded_package <- function(name) {
  if (!is.character(name) || length(name) != 1) return(FALSE)
  ga <- utils::getAnywhere(name)
  any(startsWith(ga$where, "package:"))
}

capture_function_dependencies <- function(fun, envir = globalenv(), fallback_env = parent.frame()) {
  if (!is.function(fun)) return(fun)
  body_expr <- body(fun)
  
  # Find all symbols used in the body
  find_symbols_recursive <- function(expr) {
    if (is.null(expr)) return(character())
    if (is.symbol(expr)) return(as.character(expr))
    if (is.call(expr) || is.pairlist(expr)) {
      return(unique(unlist(lapply(as.list(expr), find_symbols_recursive))))
    }
    character()
  }
  
  symbols <- find_symbols_recursive(body_expr)
  needed <- setdiff(symbols, c(names(formals(fun)), "..."))
  
  # Create new environment for function, inheriting from its original env
  old_env <- environment(fun)
  new_env <- new.env(parent = old_env)
  
  for (name in needed) {
    # Skip package-available functions
    if (is_available_from_loaded_package(name)) next
    
    obj <- tryCatch(
      get(name, envir = old_env, inherits = TRUE),
      error = function(e) tryCatch(
        get(name, envir = envir, inherits = TRUE),
        error = function(e2) tryCatch(
          get(name, envir = fallback_env, inherits = TRUE),
          error = function(e3) NULL
        )
      )
    )
    
    # Recursively capture dependencies of functions
    if (is.function(obj)) {
      obj <- capture_function_dependencies(obj, envir = envir, fallback_env = fallback_env)
    }
    
    if (!is.null(obj)) {
      assign(name, obj, envir = new_env)
    }
  }
  
  environment(fun) <- new_env
  fun
}


# Main function to capture globals for quosures
capture_globals_quosure <- 
  
  function(q, envir = globalenv(), skip = c("N"), fallback_env = parent.frame()) {
    
  if (!inherits(q, "quosure")) stop("Input must be a quosure.")
  
  expr <- rlang::quo_get_expr(q)
  old_env <- rlang::quo_get_env(q)
  
  needed <- setdiff(find_symbols_recursive(expr), skip)
  new_env <- new.env(parent = old_env)
  
  for (name in needed) {
    # Skip symbols that are from loaded packages
    if (is_available_from_loaded_package(name)) next
    
    obj <- tryCatch(
      get(name, envir = old_env, inherits = TRUE),
      error = function(e) tryCatch(
        get(name, envir = envir, inherits = TRUE),
        error = function(e2) tryCatch(
          get(name, envir = fallback_env, inherits = TRUE),
          error = function(e3) NULL
        )
      )
    )
    
    if (!is.null(obj)) {
      if (is.function(obj)) {
        obj <- capture_function_dependencies(obj, envir = envir, fallback_env = fallback_env)
      }
      assign(name, obj, envir = new_env)
    }

  }
  
  rlang::new_quosure(expr, new_env)
}


# Wrap each quosure in dots using the updated capture
# dots_add_args_quosure <- function(dots) {
#   for (i in seq_along(dots)) {
#     obj <- dots[[i]]
#     if (inherits(obj, "quosure")) {
#       dots[[i]] <- capture_globals_quosure(obj)
#     }
#   }
#   dots
# }

dots_add_args_quosure <- function(dots) {
  for (i in seq_along(dots)) {
    obj <- dots[[i]]
    
    if (inherits(obj, "quosure")) {
      dots[[i]] <- capture_globals_quosure(obj)
    } else if (is.function(obj)) {
      dots[[i]] <- capture_function_dependencies(obj)
    }
  }
  
  dots
}

# Declaration template used for all declare_* functions
declaration_template <- function(..., handler, label = NULL, pars = NULL) {
  dots <- as.list(rlang::quos(..., label = !!label))
  this <- attributes(sys.function())
  
  if (!"label" %in% names(formals(handler))) {
    dots$label <- NULL
  }
  
# Edge case: capture_function_dependencies if handler is user-defined
  if (is.function(handler)) {
    handler_env <- environment(handler)
    # if the environment is a package namespace, skip capture
    if (!isNamespace(handler_env)) {
      handler <- capture_function_dependencies(handler)
    }
  }
  
  dots <- rename_dots(handler, dots)
  dots <- dots_add_args_quosure(dots)
  
  ret <- build_step(currydata(handler, dots),
                    handler = handler,
                    dots = dots,
                    label = label,
                    step_type = this$step_type,
                    causal_type = this$causal_type,
                    call = match.call())
  
  validate(handler, ret,  dots, label)
}

# data structure for steps
build_step <- function(curried_fn, handler, dots, label, step_type, causal_type, call) {
  structure(
    curried_fn,
    handler = handler,
    dots = dots,
    label = label,
    step_type = step_type,
    causal_type = causal_type,
    call = call,
    class = c("design_step", "dd", "function")
  )
}

# generate declaration steps (eg declare_model) by setting the default handler and metadata
make_declarations <- function(default_handler, step_type, causal_type = "dgp", default_label, strictDataParam = TRUE) {
  declaration <- declaration_template

  formals(declaration)$handler <- substitute(default_handler)
  if (!missing(default_label)) {
    formals(declaration)$label <- default_label
  }

  structure(
    declaration,
    class = c("declaration", "function"),
    step_type = step_type,
    causal_type = causal_type,
    strictDataParam = strictDataParam
  )
}

###############################################################################
# internal helpers for step-specific validation code
# set on a handler (see eg reveal_outcomes_handler)
# called at declare time
#
# to debug, use debug(DeclareDesign:::validation_fn(DeclareDesign:::reveal_outcomes_handler))

validation_fn <- function(f) {
  attr(f, "validation_fn")
}

`validation_fn<-` <- with_validation_fn <- function(x, value) {
  attr(x, "validation_fn") <- value
  x
}

has_validation_fn <- function(f) {
  is.function(validation_fn(f))
}

validate <- function(handler, ret, dots, label) {
  if(is.character(label) && length(label) > 1)
    declare_time_error("Please provide only one label.", ret)
  
  if (has_validation_fn(handler)) {
    validation_fn(handler)(ret, dots, label)
  } else {
    ret
  }
}


###############################################################################
# used to inherit roxygen docs

#' @param ...      arguments to be captured, and later passed to the handler
#' @param handler  a tidy-in, tidy-out function
#' @param label    a string describing the step
#' @keywords internal
declare_internal_inherit_params <- make_declarations(function(data, ...) data.frame(BLNK = "MSG", stringsAsFactors = TRUE), step_type = "BLNKMSG")
