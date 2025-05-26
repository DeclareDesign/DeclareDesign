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
potential_outcomes_handler <- function(data,  ...) {
  
  args <- quos(...)

  # Remove quosures where the expression is exactly NULL
  args <- args[!vapply(args, function(q) is.null(quo_get_expr(q)), logical(1))]
  
  
  # Check if any argument is a formula
  is_formula <- lapply(args, function(q) is_formula(quo_squash(q))) |> 
    unlist()

  #  explicit and implicit formula arguments treated identically
  names(args)[is_formula] <- ""
  if(any(names(args)[!is_formula] == ""))  
    stop("Provide names to all non formula arguments")
  
  has_formula <- any(is_formula)
    
  # Deal with levels if supplied
  level <- args$level
  args$level <- NULL

  # If degenerate conditions are provided without an assignment var
  if(!is.null(args$conditions )){
    if(!is.list(rlang::eval_tidy(args$conditions)) & is.null(args$assignment_variables ))
      args$assignment_variables  <- rlang::quo(Z)
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
      

    if(is.null(args$conditions)){
      vars <- eval_tidy(args$assignment_variables)
      conditions <- setNames(rep(list(0:1), length(vars)), vars)
      args$conditions <- rlang::quo(conditions)
      is_formula$conditions <- FALSE
    }
   
    args$assignment_variables <- NULL
    
  }
  
  # print(names(args))
  # print(names(is_formula))
  
  
  mask <- rlang::as_data_mask(data)
  mask$N <- nrow(data)
  
  # implementation
  if (!has_formula) {   # No formula
    
    rlang::expr(fabricate(data = data, 
                          ID_label = NA, !!!args)) |>  
      rlang::eval_tidy()
    
  } else {   # With formula (usual case)

    fabricate(data = data, ID_label = NA,
            do.call(fabricatr::potential_outcomes, 
                    lapply(args, 
                           rlang::eval_tidy, 
                           data = mask)))
  }
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
 


