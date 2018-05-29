
# Helper function for "floating labels to the right" in declare_design
#
# eg declare_design(pop=declare_population(N=100))
# step 1 will have label set to pop
#
#' @importFrom rlang f_text f_env

maybe_add_labels <- function(quotations){

  labeller <- function(quotation, lbl) {
    cx <- quotation[[2]]

    if (lbl == "") {
      lbl <- f_text(quotation)
    }

    if(is.call(cx) && is.symbol(cx[[1]]) && ! "label" %in% names(cx)){

      f <- get0(as.character(cx[[1]]), f_env(quotation), "function") #match.fun does not repect quosures environment, doing get manually
      if("declaration" %in% class(f) && "label" %in% names(formals(f))){
        quotation[[2]][["label"]] <- lbl
      }
    }
    quotation
  }

  for(i in seq_along(quotations)){
    quotations[[i]] <- labeller(quotations[[i]], names(quotations)[i])
  }

  quotations
}

###############################################################################
# Helpers for declare_time checking

declare_time_error <- function(message, declaration){
  stop( simpleError(message, call = attr(declaration, "call")) )
}

declare_time_warn <- function(message, declaration){
  warning( simpleWarning(message, call = attr(declaration, "call")) )
}

declare_time_error_if_data <- function(declaration){
  if("data" %in% names(attr(declaration, "dots")))
    declare_time_error("`data` should not be a declared argument.", declaration)
}

###############################################################################
# Wrapper function, use future_lapply if we have it, fallback to lapply if not


future_lapply <- function(..., future.seed = NA, future.globals=TRUE){
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(..., future.seed=future.seed, future.globals = future.globals)
  } else {
    lapply(...)
  }
}

###############################################################################
# Descriptive statistics to be used in summary


# If <= 5 uniques, table it, ow descriptives if numeric-ish, ow number of levels.
describe_variable <- function(x) {

  num_unique=length(unique(x))

  if(num_unique > 5) {
    return(describe_variable_impl(x, num_unique))
  }

  tab <- table(x, exclude = NULL)

  rbind.data.frame(
    Frequency=data.frame(as.list(tab), check.names = FALSE),
    Proportion=sprintf("%.2f", prop.table(tab))
  )

}


describe_variable_impl <- function(x, num_unique) UseMethod("describe_variable_impl")

describe_variable_impl.factor <- function(x, num_unique) {
  data.frame(
    as.list(summary.factor(x, 5)),
    N_unique=num_unique,
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
      list(min=min, median=median, mean=mean, max=max, sd=sd),
      function(f,x) round(f(x, na.rm=TRUE), digits=2),
      x
    ),
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


###############################################################################

# Like rbind, but fill missing columns with NA
rbind_disjoint <- function(list_of_df, infill=NA) {
  list_of_df <- Filter(is.data.frame, list_of_df)
  all_columns <- Reduce(union, lapply(list_of_df, colnames))

  for(i in seq_along(list_of_df)) {
    list_of_df[[i]][setdiff(all_columns, colnames(list_of_df[[i]]))] <- infill
  }

  list_of_df <- lapply(list_of_df, `[`, all_columns)

  do.call(rbind.data.frame, append(list_of_df, list(make.row.names=FALSE, stringsAsFactors=FALSE)))
}


`%i%` <- intersect

`%||%` <- function(e1, e2) if(is.null(e1)) e2 else e1

add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  return_vec <- sprintf("(%s)", format_num(x, digits))
  return(return_vec)
}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}


check_design_estimator_labels <- function(design) any(duplicated(run_design(design)[[1]]$estimator_label))

