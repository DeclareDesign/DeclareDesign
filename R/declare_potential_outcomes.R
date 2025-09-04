#' uses fabricatr::potential_outcomes
#. but also allows deprecated case where values are provided directly
#' @param data A data.frame.
#' @importFrom rlang quos !!!
#' @importFrom fabricatr fabricate
#' @rdname declare_measurement
#'
#'
#'
#'
potential_outcomes_handler <- function(data, ...) {
  args <- quos(...)
  
  # Remove quosures where the expression is exactly NULL
  args <- args[!vapply(args, function(q)
    is.null(quo_get_expr(q)), logical(1))]
  
  
  # Check if any argument is a formula
  is_formula <- lapply(args, function(q)
    is_formula(quo_squash(q))) |>
    unlist()
  
  #  explicit and implicit formula arguments treated identically
  names(args)[is_formula] <- ""
  if (any(names(args)[!is_formula] == ""))
    stop("Provide names to all non formula arguments")
  
  has_formula <- any(is_formula)
  
  # Deal with levels if supplied
  level <- args$level
  args$level <- NULL
  
  # If degenerate conditions are provided without an assignment var
  if (!is.null(args$conditions)) {
    if (!is.list(rlang::eval_tidy(args$conditions)) &
        is.null(args$assignment_variables)) {
      args$assignment_variables  <- rlang::quo(Z)
    }
  }
  
  
  # Ignore assignment_variables and create conditions if needed
  if ("assignment_variables" %in% names(args)) {
    # message("assignment_variables is deprecated")
    
    # cases like:  conditions = c(1,2,3) and assignment_variables = "Z"
    assignment_quo <- args$assignment_variables
    conditions_quo <- args$conditions
    
    
    # Check whether conditions is NOT a list
    is_not_list <- !is.list(rlang::eval_tidy(conditions_quo))
    
    if (!is.null(conditions_quo) &&
        is_not_list &&
        rlang::is_symbol(rlang::quo_squash(assignment_quo))) {
      var <- rlang::as_label(rlang::quo_squash(assignment_quo))
      condition_value <- rlang::eval_tidy(conditions_quo)
      
      conditions <- setNames(list(condition_value), var)
      args$conditions <- rlang::quo(conditions)
    }
    
    
    if (is.null(args$conditions)) {
      vars <- eval_tidy(args$assignment_variables)
      conditions <- setNames(rep(list(0:1), length(vars)), vars)
      args$conditions <- rlang::quo(conditions)
    }
    
    args$assignment_variables <- NULL
    
  }
  
  
  # default conditions argument
  if (is.null(args$conditions)  & has_formula) {
    args$conditions <- rlang::quo(list(Z = c(0, 1)))
  }
  
  mask <- rlang::as_data_mask(data)
  mask$N <- nrow(data)
  
  # implementation
  if (!has_formula) {
    # No formula
    out <-   rlang::expr(fabricate(data = data, ID_label = NA, !!!args)) |>
      rlang::eval_tidy()
    
  } else {
    # With formula (usual case)
    
    # there is a lot more work here to ensure environments OK than with
    # the nondeprecaated  declare_ functions
    
    # evaluate args
    args_eval <- lapply(args, rlang::eval_tidy, data = mask)
    
    # find the formula arg
    is_form <- vapply(args, function(q)
      rlang::is_formula(rlang::quo_squash(q)), logical(1))
    f_idx   <- which(is_form)[1L]
    
    # evaluated formula
    f  <- args_eval[[f_idx]]
    stopifnot(rlang::is_formula(f))
    
    # original quosure env (captured globals like outcome_means, sd, etc.)
    f_quo <- args[[f_idx]]
    qe    <- rlang::quo_get_env(f_quo)
    
    # 1) Build a plain environment that contains DATA columns (and N)
    data_env <- list2env(c(as.list(data), list(N = nrow(data))), parent = qe)
    
    # 2) Writable env for fabricatr; parent = data_env keeps columns visible, globals next
    safe_env <- new.env(parent = data_env)
    
    # 3) Attach and call fabricatr
    rlang::f_env(f) <- safe_env
    args_eval[[f_idx]] <- f
    
    out <- fabricate(
      data = data,
      ID_label = NA,
      do.call(fabricatr::potential_outcomes, args_eval)
    )
  }
  
  out
}


#' Declare potential outcomes
#'
#' Deprecated. Please use the potential_outcomes function within a declare_model declaration.
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that returns a data.frame
#'
#' @export
#'
#' @keywords internal
#'
declare_potential_outcomes <-
  make_declarations(potential_outcomes_handler, "potential_outcomes")


# validation_fn(potential_outcomes_handler) <- function(ret, dots, label) {
#   ret
#   }
