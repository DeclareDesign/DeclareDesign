#' Declare potential outcomes
#'
#' @inheritParams declare_internal_inherit_params
#'
#' @return a function that returns a data.frame
#'
#' @export
#'
#' @details
#'
#' A \code{declare_potential_outcomes} declaration returns a function. The function takes and returns a data.frame with potential outcomes columns appended. These columns describe the outcomes that each unit would express if that unit were in the corresponding treatment condition.
#'
#' Declaring a potential outcomes function requires postulating a particular causal process. One can then assess how designs fare under the postulated process. 
#' Multiple processes can be considered in a single design or across design. For instance one could declare two processes that rival theories would predict.
#' 
#' Potential outcomes can be declared as separate variables or by using a formula. See examples below.
#'
#' @examples
#'
#' # Declare potential outcomes using default handler
#'
#' # There are two ways of declaring potential outcomes:
#'
#' # As separate variables
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = .05,
#'   Y_Z_1 = .30 + .01 * age
#' )
#'
#' # Using a formula
#'  my_potential_outcomes <- declare_potential_outcomes(
#'    Y ~ .05 + .25 * Z + .01 * age * Z)
#'
#' # `conditions` defines the "range" of the potential outcomes function
#'  my_potential_outcomes <- declare_potential_outcomes(
#'    formula = Y ~ .05 + .25 * Z + .01 * age * Z,
#'    conditions = 1:4
#'  )
#'
#' # Multiple assignment variables can be specified in `conditions`. For example,
#' # in a 2x2 factorial potential outcome:
#'
#'  my_potential_outcomes <- declare_potential_outcomes(
#'    formula = Y ~ .05 + .25 * Z1 + .01 * age * Z2,
#'    conditions = list(Z1 = 0:1, Z2 = 0:1)
#'  )
#'
#' # You can also declare potential outcomes using a custom handler
#'
#' my_po_function <- function(data) {
#'   data$Y_treated   <- rexp(nrow(data), .2)
#'   data$Y_untreated <- rexp(nrow(data), .4)
#'   data
#' }
#'
#' custom_potential <- declare_potential_outcomes(handler = my_po_function)
#'
declare_potential_outcomes <- make_declarations(potential_outcomes_handler, "potential_outcomes")
### Default handler calls either the formula handler or non-formula handler
### this can be determind at declare time in the validation_fn, and the correct function returned instead
### If possible, we do so, even though much of the logic is essentially duplicated
### this makes tracing the execution in run_design much simpler

potential_outcomes_handler <- function(..., data, level) {
  (function(formula, ...) UseMethod("potential_outcomes"))(..., data = data, level = level)
}

validation_fn(potential_outcomes_handler) <- function(ret, dots, label) {
  declare_time_error_if_data(ret)

  # Below is a similar redispatch strategy, only at declare time
  validation_delegate <- function(formula = NULL, ...) {
    potential_outcomes <- function(formula, ...) UseMethod("potential_outcomes", formula)
    for (c in class(formula)) {
      s3method <- getS3method("potential_outcomes", class(formula))
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
    currydata(s3method, dots, strictDataParam = attr(ret, "strictDataParam")),
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
potential_outcomes.formula <- function(formula,
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


validation_fn(potential_outcomes.formula) <- function(ret, dots, label) {
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

  ret <- build_step(currydata(potential_outcomes.formula,
    dots,
    strictDataParam = attr(ret, "strictDataParam"),
    cloneDots = FALSE
  ),
  handler = potential_outcomes.formula,
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
    ),
    design_validation = pofdv
  )
}


# A design time validation
#
#  Checks for unrevealed outcome variables.
#
#  If there are any, inject a declare_reveal step after the latest assign/reveal of an assn variable
#
#
pofdv <- function(design, i, step) {
  if (i == length(design)) {
    return(design)
  }

  this_step_meta <- attr(step, "step_meta")

  check <- function(var_type, step_type, step_attr, callback = identity, from = 1, to = length(design)) {
    vars <- this_step_meta[[var_type]]

    assn_steps <- Filter(
      function(step_j) attr(step_j, "step_type") == step_type,
      design[from:to]
    )

    for (step_j in assn_steps) {
      if (is.null(step_meta <- attr(step_j, "step_meta"))) next
      step_assn <- step_meta[[step_attr]]
      vars <- setdiff(vars, step_assn)
      if (length(vars) == 0) return(c())
    }

    callback(vars)
  }

  unrevealed_outcomes <- check("outcome_variables", "reveal", "outcome_variables",
    from = i + 1,
    function(vars) {
      vars
    }
  )

  if (length(unrevealed_outcomes) == 0) return(design)

  # warning(
  #   "Outcome variables (", paste(unrevealed_outcomes, sep = ", "),
  #   ") were declared in a potential outcomes step (", attr(step, "label"),
  #   "), but never later revealed.", call. = FALSE)

  prev_unassigned <- check("assignment_variables", "assignment", "assignment_variables", to = i - 1)
  prev_unrevealed <- check("assignment_variables", "reveal", "outcome_variables", to = i - 1)

  if (length(prev_unassigned %i% prev_unrevealed) == 0) {
    new_step <- eval_tidy(quo(declare_reveal(
      outcome_variables = !!this_step_meta$outcome_variables,
      assignment_variables = !!this_step_meta$assignment_variables,
      label = !!paste("Autogenerated by", attr(step, "label"))
    )))
    attr(new_step, "auto-generated") <- TRUE

    # warning("Attempting to inject a `declare_reveal(", this_step_meta$outcome_variables, ", ",
    #         this_step_meta$assignment_variables,
    #         ")` step after PO (", attr(step, "label"),
    #         ")", call. = FALSE)

    design <- insert_step(design, new_step, after = i)
    return(design)
  }

  unassigned_vars <- check("assignment_variables", "assignment", "assignment_variables", from = i + 1)
  unrevealed_vars <- check("assignment_variables", "reveal", "outcome_variables", from = i + 1)

  cant_find <- prev_unassigned %i% prev_unrevealed %i% unassigned_vars %i% unrevealed_vars


  new_step <- eval_tidy(quo(declare_reveal(
    outcome_variables = !!this_step_meta$outcome_variables,
    assignment_variables = !!this_step_meta$assignment_variables,
    label = !!paste("Autogenerated by", attr(step, "label"))
  )))
  attr(new_step, "auto-generated") <- TRUE

  for (step_j in design[length(design):(i + 1)]) {
    if (is.null(step_meta <- attr(step_j, "step_meta"))) next
    if (attr(step_j, "step_type") == "assignment") {
      if (any(step_meta$assignment_variables %in% attr(step, "step_meta")$assignment_variables)) {
        design <- insert_step(design, new_step, after = step_j)
        break
      }
    }
    else if (attr(step_j, "step_type") == "reveal") {
      if (any(step_meta$outcome_variables %in% attr(step, "step_meta")$assignment_variables)) {
        design <- insert_step(design, new_step, after = step_j)
        break
      }
    }
  }

  design
}



#' @importFrom fabricatr fabricate add_level modify_level
#' @rdname declare_potential_outcomes
potential_outcomes.NULL <- function(formula = stop("Not provided"), ..., data, level = NULL) {
  if (is.character(level)) {
    fabricate(data = data, !!level := modify_level(...))
  } else {
    fabricate(data = data, ..., ID_label = NA)
  }
}

validation_fn(potential_outcomes.NULL) <- function(ret, dots, label) {
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
formals(expand_conditions) <- formals(potential_outcomes.formula)
formals(expand_conditions)["label"] <- list(NULL) # Fixes R CMD Check warning outcome is undefined
