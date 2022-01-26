###############################################################################
# Copies an environment chain
#' @importFrom rlang env_clone
env_deep_copy <- function(e) {
  # Cloning the CheckExEnv causes examples to autofail, it has delayedAssign("F", stop())
  if (environmentName(e) == "CheckExEnv") {
    e
  } else if (identical(e, emptyenv())) {
    emptyenv()
  } else if (identical(e, globalenv())) {
    env_clone(e)
  } else {
    env_clone(e, Recall(parent.env(e)))
  }
  # don't clone attached packages
}

###############################################################################
# For set of dots, copy environment chain, reusing the new env if possible
# to save memory
#' @importFrom rlang env_clone

dots_env_copy <- function(dots) {
  eprev <- NULL
  for (i in seq_along(dots)) {
    ecurrent <- environment(dots[[i]])
    if (!is.null(ecurrent)) {
      if (!identical(ecurrent, eprev)) {
        eprev <- ecurrent
        eclone <- env_deep_copy(ecurrent)
      }
      environment(dots[[i]]) <- eclone
    }
  }
  dots
}


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

# Implementation for declarations
# captures the dots and handler, and returns a function that calls the handler with dots
# also deals with labeling and can trigger step validation
#' @importFrom rlang enquo
declaration_template <- function(..., handler, label = NULL) {
  # message("Declared")

  dots <- as.list(quos(..., label = !!label))
  this <- attributes(sys.function())

  if (!"label" %in% names(formals(handler))) {
    dots$label <- NULL
  }

  dots <- rename_dots(handler, dots)
  dots <- dots_env_copy(dots)


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
