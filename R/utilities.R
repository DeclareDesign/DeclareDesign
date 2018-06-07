
# Helper function for "floating labels to the right" in declare_design
#
# eg declare_design(pop=declare_population(N=100))
# step 1 will have label set to pop
#
#' @importFrom rlang f_text f_env
maybe_add_labels <- function(quotations) {
  
  labeller <- function(quotation, lbl) {
    cx <- quotation[[2]]
    
    if (lbl == "") {
      lbl <- f_text(quotation)
    }
    
    if (is.call(cx) && is.symbol(cx[[1]]) && !"label" %in% names(cx)) {
      
      f <- get0(as.character(cx[[1]]), f_env(quotation), "function") #match.fun does not repect quosures environment, doing get manually
      if ("declaration" %in% class(f) && "label" %in% names(formals(f))) {
        quotation[[2]][["label"]] <- lbl
      }
    }
    quotation
  }
  
  for (i in seq_along(quotations)) {
    if (names(quotations)[i] == "")
      names(quotations)[i] <- f_text(quotations[[i]])
    quotations[[i]] <- labeller(quotations[[i]], names(quotations)[i])
  }
  
  if (any(duplicated(names(quotations)))) {
    stop(paste0("Please provide unique names for each design step. Duplicates include ", 
                paste(names(quotations)[duplicated(names(quotations))], collapse = ", "), 
                ". You can name steps within declare design, e.g. declare_design(my_pop = pop_step)."), call. = FALSE)
  }
  
  quotations
}

###############################################################################
# Helpers for declare_time checking

declare_time_error <- function(message, declaration) {
  stop( simpleError(message, call = attr(declaration, "call")) )
}

declare_time_warn <- function(message, declaration) {
  warning( simpleWarning(message, call = attr(declaration, "call")) )
}

declare_time_error_if_data <- function(declaration) {
  if ("data" %in% names(attr(declaration, "dots")))
    declare_time_error("`data` should not be a declared argument.", declaration)
}

###############################################################################
# Wrapper function, use future_lapply if we have it, fallback to lapply if not


future_lapply <- function(..., future.seed = NA, future.globals=TRUE) {
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(..., future.seed = future.seed, future.globals = future.globals)
  } else {
    lapply(...)
  }
}

# Descriptive statistics to be used in summary


# If <= 5 uniques, table it, ow descriptives if numeric-ish, ow number of levels.
describe_variable <- function(x) {
  
  num_unique <- length(unique(x))
  
  if (num_unique > 5) {
    return(describe_variable_impl(x, num_unique))
  }
  
  tab <- table(x, exclude = NULL)
  
  rbind.data.frame(
    Frequency = data.frame(as.list(tab), check.names = FALSE),
    Proportion = sprintf("%.2f", prop.table(tab))
  )
  
}


describe_variable_impl <- function(x, num_unique) UseMethod("describe_variable_impl")

describe_variable_impl.factor <- function(x, num_unique) {
  data.frame(
    as.list(summary.factor(x, 5)),
    N_unique = num_unique,
    check.names = FALSE
  )
}

describe_variable_impl.character <-  function(x, num_unique) {
  data.frame(
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    class = class(x),
    stringsAsFactors = FALSE
  )
}

#' @importFrom stats median sd
describe_variable_impl.default <- function(x, num_unique) {
  #todo just use summary?
  data.frame(
    lapply(
      list(
        min = min,
        median = median,
        mean = mean,
        max = max,
        sd = sd
      ),
      function(f, x) round(f(x, na.rm = TRUE), digits = 2),
      x
    ),
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}



# Like rbind, but fill missing columns with NA
rbind_disjoint <- function(list_of_df, infill=NA) {
  list_of_df <- Filter(is.data.frame, list_of_df)
  all_columns <- Reduce(union, lapply(list_of_df, colnames))
  
  for (i in seq_along(list_of_df)) {
    list_of_df[[i]][setdiff(all_columns, colnames(list_of_df[[i]]))] <- infill
  }
  
  list_of_df <- lapply(list_of_df, `[`, all_columns)
  
  do.call(rbind.data.frame, append(list_of_df, list(make.row.names = FALSE, stringsAsFactors = FALSE)))
}

add_parens <- function(x, digits = 3) {
  sprintf("(%s)", format_num(x, digits))
}

format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

# Function to check whether there are more sims run than expected, possibly because of repeated labels
check_sim_number <- function(simulations_df,
                             sims,
                             grouping_variables = c("design_label",  "estimand_label", "estimator_label", "coefficient")) {
  
  group_by_set  <- colnames(simulations_df) %i% grouping_variables
  group_by_list <- simulations_df[, group_by_set, drop = FALSE]
  check_df      <- split(group_by_list, group_by_list, drop = TRUE)
  check_sims    <- unlist(lapply(check_df, nrow)) != prod(sims)
  if (any(check_sims)) {
    warning(paste0("More simulations than expected for profiles:",
                   paste0(names(check_sims)[check_sims], collapse = ", ")))
  }
}


# helpers for summary.design

get_added_variables <- function(last_df = NULL, current_df) {
  setdiff(names(current_df), names(last_df))
}

get_modified_variables <- function(last_df = NULL, current_df) {
  is_modified <- function(j) !isTRUE(all.equal(last_df[[j]], current_df[[j]]))
  shared <- intersect(names(current_df), names(last_df))
  
  Filter(is_modified, shared)
}


###############################################################################
## Helper functions for declaratiosn that should work either with symbols,
## string literals, or functions of either
## eg Y:Z => c("Y","Z")

reveal_nse_helper <- function(X) {
  if (is.character(X) || is.logical(X))
    X
  else if (is.name(X))
    as.character(X)
  else if (is_quosure(X))
    reveal_nse_helper(quo_expr(X))
  else if (is.call(X))
    unlist(lapply(X[-1], reveal_nse_helper))
}

reveal_nse_helper_dots <- function(dots, what, handler) {
  if (what %in% names(dots)) {
    dots[[what]] <- reveal_nse_helper(dots[[what]])
  } else if (!is.null(formals(handler)[[what]])) {
    dots[[what]] <- as.character(formals(handler)[[what]])
  }
  
  dots
}


