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
declare_potential_outcomes <- make_declarations(potential_outcomes_handler, "potential_outcomes")

potential_outcomes_handler <- function(..., data, level) {
  (function(formula, ...) UseMethod("potential_outcomes_internal"))(..., data = data, level = level)
}

validation_fn(potential_outcomes_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  # Below is a similar redispatch strategy, only at declare time
  validation_delegate <- function(formula = NULL, ...) {
    potential_outcomes_internal <- function(formula, ...) UseMethod("potential_outcomes_internal", formula)
    for (c in class(formula)) {
      s3method <- getS3method("potential_outcomes_internal", class(formula))
      if (is.function(s3method)) return(s3method)
    }
    declare_time_error("Could not find appropriate implementation", ret)
  }

  s3method <- eval_tidy(quo(validation_delegate(!!!dots)))

  # explicitly name all dots, for easier s3 handler validation
  dots <- rename_dots(s3method, dots)

  if ("level" %in% names(dots)) {
    dots$level <- reveal_nse_helper(dots$level)
  }


  ret <- build_step(
    currydata(s3method, dots),
    handler = s3method,
    dots = dots,
    label = label,
    step_type = attr(ret, "step_type"),
    causal_type = attr(ret, "causal_type"),
    call = attr(ret, "call")
  )

  if (has_validation_fn(s3method)) ret <- validate(s3method, ret, dots, label)

  ret
}

#' @param formula a formula to calculate potential outcomes as functions of assignment variables.
#' @param conditions see \code{\link{expand_conditions}}. Provide values (e.g. \code{conditions = 1:4}) for a single assignment variable. If multiple assignment variables, provide named list (e.g. \code{conditions = list(Z1 = 0:1, Z2 = 0:1)}). Defaults to 0:1 if no conditions provided.
#' @param assignment_variables The name of the assignment variable. Generally not required as names are taken from \code{conditions}.
#' @param level a character specifying a level of hierarchy for fabricate to calculate at
#' @param data a data.frame
#' @importFrom fabricatr fabricate
#' @importFrom rlang quos := !! !!! as_quosure
#' @rdname declare_potential_outcomes
potential_outcomes_internal.formula <- function(formula,
                                       conditions = c(0, 1),
                                       assignment_variables = "Z", # only used to provide a default - read from names of conditions immediately after.
                                       data,
                                       level = NULL,
                                       label = outcome_variable) {
  outcome_variable <- as.character(formula[[2]])

  to_restore <- assignment_variables %icn% data
  to_null <- setdiff(assignment_variables, to_restore)

  # Build a single large fabricate call -
  # fabricate( Z=1, Y_Z_1=f(Z), Z=2, Y_Z_2=f(Z), ..., Z=NULL)
  condition_quos <- quos()

  ### If assn vars already present, swap them out
  if (length(to_restore) > 0) {
    restore_mangled <- paste(rep("_", max(nchar(colnames(data)))), collapse = "")

    restore_mangled <- setNames(
      lapply(to_restore, as.symbol),
      paste0(".", restore_mangled, to_restore)
    )

    condition_quos <- c(condition_quos, quos(!!!restore_mangled))
  }

  # build call
  expr <- as_quosure(formula)
  for (i in seq_len(nrow(conditions))) {
    condition_values <- conditions[i, , drop = FALSE]
    out_name <- paste0(outcome_variable, "_", paste0(assignment_variables, "_", condition_values, collapse = "_"))

    condition_quos <- c(condition_quos, quos(!!!condition_values, !!out_name := !!expr))
  }

  # clean up
  if (length(to_restore) > 0) {
    to_restore <- setNames(
      lapply(names(restore_mangled), as.symbol),
      to_restore
    )
    restore_mangled <- lapply(restore_mangled, function(x) NULL)
    condition_quos <- c(condition_quos, quos(!!!to_restore), quos(!!!restore_mangled))
  }

  if (length(to_null) > 0) {
    to_null <- lapply(setNames(nm = to_null), function(x) NULL)
    condition_quos <- c(condition_quos, quos(!!!to_null))
  }


  if (is.character(level)) {
    condition_quos <- quos(!!level := modify_level(!!!condition_quos))
  }

  ### Actually do it and return
  ### Note ID_label=NA
  structure(
    fabricate(data = data, !!!condition_quos, ID_label = NA),
    outcome_variable = outcome_variable,
    assignment_variables = assignment_variables
  )
}


validation_fn(potential_outcomes_internal.formula) <- function(ret, dots, label) {
  dots$formula <- eval_tidy(dots$formula)
  outcome_variable <- as.character(dots$formula[[2]])

  if (length(dots$formula) < 3) {
    declare_time_error("Must provide an outcome in potential outcomes formula", ret)
  }

  if ("ID_label" %in% names(dots)) {
    declare_time_error("Must not pass ID_label.", ret)
  }

  if ("assignment_variables" %in% names(dots)) {
    dots$assignment_variables <- reveal_nse_helper(dots$assignment_variables)
  }

  dots$conditions <- eval_tidy(quo(expand_conditions(!!!dots)))
  dots$assignment_variables <- names(dots$conditions)

  ret <- build_step(currydata(potential_outcomes_internal.formula,
    dots),
  handler = potential_outcomes_internal.formula,
  dots = dots,
  label = label,
  step_type = attr(ret, "step_type"),
  causal_type = attr(ret, "causal_type"),
  call = attr(ret, "call")
  )


  # Note that this sets a design_validation callback for later use!!! see below
  # step_meta is the data that design_validation will use for design time checks
  structure(ret,
    potential_outcomes_formula = formula,
    step_meta = list(
      outcome_variables = outcome_variable,
      assignment_variables = names(dots$conditions)
    )
  )
}


#' @importFrom fabricatr fabricate add_level modify_level
#' @rdname declare_potential_outcomes
potential_outcomes_internal.NULL <- function(formula = stop("Not provided"), ..., data, level = NULL) {
  if (is.character(level)) {
    fabricate(data = data, !!level := modify_level(...))
  } else {
    fabricate(data = data, ..., ID_label = NA)
  }
}

validation_fn(potential_outcomes_internal.NULL) <- function(ret, dots, label) {
  if ("ID_label" %in% names(dots)) {
    declare_time_error("Must not pass ID_label.", ret)
  }

  if ("" %in% names(dots)) {
    declare_time_warn("Unnamed declared argument in potential outcome", ret)
  }

  ret
}




#' Expand assignment conditions
#'
#' Internal helper to eagerly build assignment conditions for potential outcomes.
#'
#' If conditions is a data.frame, it is returned unchanged
#'
#' Otherwise, if conditions is a list, it is passed to expand.grid for expansion to a data.frame
#'
#' Otherwise, if condition is something else, box it in a list with assignment_variables for names,
#' and pass that to expand.grid.
#'
#' @param conditions the conditions
#' @param assignment_variables the name of assignment variables, if conditions is not already named.
#' @return a data.frame of potential outcome conditions
#' @keywords internal
expand_conditions <- function() {
  if (!is.data.frame(conditions)) {
    if (!is.list(conditions)) {
      conditions <- rep(list(conditions), length(assignment_variables))
      conditions <- setNames(conditions, assignment_variables)
    }

    conditions <- expand.grid(conditions, stringsAsFactors = FALSE)
  }
  conditions
}
formals(expand_conditions) <- formals(potential_outcomes_internal.formula)
formals(expand_conditions)["label"] <- list(NULL) # Fixes R CMD Check warning outcome is undefined
