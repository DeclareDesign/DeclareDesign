## this function is from lazyeval version git version, commit c155c3d
freeze_environment <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

from_package <- function(func, package) {
  func_package <-
    tryCatch(
      getNamespaceName(environment(func)),
      error = function(e)
        NULL
    )
  ifelse(is.null(func_package), FALSE, func_package == package)
}

#' @importFrom rlang quos lang_fn lang_modify eval_tidy
wrap_step <- function(...) {
  ## this function allows you to put any R expression
  ## such a dplyr call mutate
  ## into the causal order, i.e.
  ## declare_design(pop(), po, declare_step(mutate(q = 5)))

  step_call <- quos(...)[[1]]

  arg_names <- names(formals(lang_fn(step_call)))

  declare_step_function_internal <- function(data) {
    if (".data" %in% arg_names) {
      step_call <- lang_modify(step_call, .data = data)
    } else if ("data" %in% arg_names) {
      step_call <- lang_modify(step_call, data = data)
    }
    eval_tidy(step_call)
  }

  attributes(declare_step_function_internal) <-
    list(call = match.call(), type = "declare_step")

  return(declare_step_function_internal)

}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

#' @importFrom stats median sd
describe_variable <- function(x) {
  num_unique <- length(unique(x))
  num_missing <- sum(is.na(x))

  if (num_unique <= 5) {
    tab <- table(x, exclude = NULL)
    prop_tab <- prop.table(tab)

    df <-
      cbind(
        data.frame(tab, stringsAsFactors = FALSE)[, 2],
        data.frame(prop_tab, stringsAsFactors = FALSE)[, 2]
      )

    obj <- data.frame(t(df), stringsAsFactors = FALSE)
    obj[1, ] <- format_num(obj[1, ], digits = 0)
    obj[2, ] <- format_num(obj[2, ], digits = 2)
    obj <- cbind(c("Frequency", "Proportion"), obj)
    colnames(obj) <- c("", names(tab))

  } else if ((typeof(x) == "character" ||
              (typeof(x) == "integer" &&
               class(x) == "factor")) & num_unique > 5) {
    obj <- data.frame(
      N_missing = num_missing,
      N_unique = num_unique,
      stringsAsFactors = FALSE
    )

  } else {
    obj <- data.frame(
      min = min(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      N_missing = num_missing,
      N_unique = num_unique,
      stringsAsFactors = FALSE
    )
    obj[, c("min", "median", "mean", "max", "sd")] <-
      apply(obj[, c("min", "median", "mean", "max", "sd")], 2, format_num, digits = 2)
  }

  rownames(obj) <- NULL
  return(obj)
}

get_unique_variables_by_level <- function(data, ID_label) {
  ## identify variables that do not vary within ID_label
  ## maybe there is a faster way to do this?
  level_variables <-
    sapply(colnames(data)[!colnames(data) %in% ID_label], function(i)
      max(tapply(data[, i], list(data[, ID_label]),
                 function(x)
                   length(unique(x)))) == 1)
  return(names(level_variables)[level_variables])
}
