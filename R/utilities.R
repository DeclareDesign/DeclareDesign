

#' @importFrom rlang f_text f_env
maybe_add_names_qs <- function(quotations) {
  namer <- function(quotation) {
    cx <- quotation[[2]]

    if (is.call(cx) && is.symbol(cx[[1]])) {
      f <- get0(as.character(cx[[1]]), f_env(quotation), "function") # match.fun does not repect quosures environment, doing get manually
      if ("declaration" %in% class(f)) {
        if (!is.null(quotation[[2]][["label"]])) {
          nm <- quotation[[2]][["label"]]
        } else {
          nm <- ""
        }
      } else {
        nm <- f_text(quotation)
      }
    } else {
      # TODO: want to move labels into names
      #  this seems like a terrible way to do so
      obj <- eval_tidy(quotation)
      lbl <- attr(obj, "label")
      if (!is.null(lbl)) {
        nm <- lbl
      } else {
        nm <- f_text(quotation)
      }
    }
  }

  for (i in seq_along(quotations)) {
    if (names(quotations)[i] == "") {
      names(quotations)[i] <- namer(quotations[[i]])
    }
  }

  quotations
}


# Helpers for declare_time checking ---------------------------------------


declare_time_error <- function(message, declaration) {
  stop(simpleError(message, call = attr(declaration, "call")))
}

declare_time_warn <- function(message, declaration) {
  warning(simpleWarning(message, call = attr(declaration, "call")))
}

declare_time_error_if_data <- function(declaration) {
  if ("data" %in% names(attr(declaration, "dots"))) {
    declare_time_error("`data` should not be a declared argument.", declaration)
  }
}


# Wrapper function, use future_lapply if we have it, fallback to lapply if not

future_lapply <- function(..., future.seed = NA, future.globals = TRUE) {
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(..., future.seed = future.seed, future.globals = future.globals)
  } else {
    lapply(...)
  }
}


#  Descriptive statistics to be used in summary ---------------------------

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

describe_variable_impl.POSIXct <- function(x, num_unique) {
  data.frame(
    as.list(as.character(summary(x))),
    N_unique = num_unique,
    check.names = FALSE
  )
}


describe_variable_impl.character <- function(x, num_unique) {
  data.frame(
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    class = class(x),
    stringsAsFactors = FALSE
  )
}

#' @importFrom stats median sd
describe_variable_impl.default <- function(x, num_unique) {
  # todo just use summary?
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
rbind_disjoint <- function(list_of_df, infill = NA) {
  list_of_df <- Filter(is.data.frame, list_of_df)
  all_columns <- Reduce(union, lapply(list_of_df, colnames))

  for (i in seq_along(list_of_df)) {
    list_of_df[[i]][setdiff(all_columns, colnames(list_of_df[[i]]))] <- infill
  }

  list_of_df <- lapply(list_of_df, `[`, all_columns)

  do.call(rbind.data.frame, append(list_of_df, list(make.row.names = FALSE, stringsAsFactors = FALSE)))
}

# Formatting --------------------------------------------------------------

add_parens <- function(x, digits = 3) {
  ret <- sprintf("(%s)", format_num(x, digits))
  ret[is.na(x)] <- "NA"
  ret
}

format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

# helpers for summary.design ----------------------------------------------

get_added_variables <- function(last_df = NULL, current_df) {
  setdiff(names(current_df), names(last_df))
}

get_modified_variables <- function(last_df = NULL, current_df) {
  is_modified <- function(j) !isTRUE(all.equal(last_df[[j]], current_df[[j]]))
  shared <- intersect(names(current_df), names(last_df))

  Filter(is_modified, shared)
}


# Helper functions for declarations that should work either with symbols,
# string literals, or functions of either
# eg Y:Z => c("Y","Z")

#' @importFrom rlang quo_squash
reveal_nse_helper <- function(X) {
  if (is.character(X) || is.logical(X)) {
    X
  } else if (is.name(X)) {
    as.character(X)
  } else if (is_quosure(X)) {
    reveal_nse_helper(quo_squash(X))
  } else if (is.call(X)) {
    unlist(lapply(X[-1], reveal_nse_helper))
  }
}

reveal_nse_helper_dots <- function(dots, what, handler) {
  if (what %in% names(dots)) {
    dots[[what]] <- reveal_nse_helper(dots[[what]])
  } else if (!is.null(formals(handler)[[what]])) {
    dots[[what]] <- as.character(formals(handler)[[what]])
  }

  dots
}


is_autogenerated <- function(x) !is.null(attr(x, "auto-generated"))


step_type <- function(x) UseMethod("step_type", x)

step_type.design_step <- function(x) attr(x, "step_type")
step_type.function <- function(x) "unknown"
step_type.default <- function(x) "unknown"

#' @importFrom rlang is_symbol expr_name
wrap_step <- function(step, expr) {
  expr_txt <- if (is_symbol(expr)) expr_name(expr)
  nm <- attr(step, "label") %||% expr_txt %||% step_type(step)
  if (is.null(attr(step, "call"))) attr(step, "call") <- expr
  structure(setNames(list(step), nm), valid = !is.null(expr_txt))
}
