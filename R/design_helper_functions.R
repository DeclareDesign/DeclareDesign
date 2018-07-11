#' Explore your design
#'
#' @param design A design object, typically created using the + operator
#'
#' @examples
#'
#' design <-
#'   declare_population(N = 500, noise = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ noise + Z * rnorm(N, 2, 2)) +
#'   declare_sampling(n = 250) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_step(dplyr::mutate, noise_sq = noise^2) +
#'   declare_assignment(m = 25) +
#'   declare_reveal() +
#'   declare_estimator(Y ~ Z, estimand = "my_estimand")
#'
#' design
#'
#' df <- draw_data(design)
#'
#' estimates <- get_estimates(design)
#' estimands <- get_estimands(design)
#'
#' @name post_design
NULL


# For fan-out execution, convert the vector representation to (end, n) pairs

check_sims <- function(design, sims) {
  n <- length(design)
  if (!is.data.frame(sims)) {
    if (length(sims) == n) {
      sims_full <- sims
    }
    else if (is.character(names(sims))) {
      sims_full <- rep(1, n)
      design_labels <- as.character(lapply(design, attr, "label"))
      i <- match(names(sims), design_labels)
      sims_full[i] <- sims
    } else if (length(sims) != n) {
      sims_full <- c(sims, rep(1, n))[1:n]
    }

    ret <- data.frame(end = 1:n, n = sims_full)
  }

  # Compress sequences of ones into one partial execution
  include <- rep(TRUE, n)
  last_1 <- FALSE
  for (i in n:1) {
    if (!last_1) {
      include[i] <- TRUE
    } else if (ret[i, "n"] == 1 && last_1) include[i] <- FALSE

    last_1 <- ret[i, "n"] == 1
  }

  ret[include, , drop = FALSE]
}

#' Execute a design
#'
#' @param design a DeclareDesign object
#'
#' @export
run_design <- function(design) run_design_internal(design)

run_design_internal <- function(design, ...) UseMethod("run_design_internal", design)

next_step <- function(step, current_df, i) {
  tryCatch(
    nxt <- step(current_df),
    error = function(err) {
      stop(simpleError(sprintf("Error in step %d (%s):\n\t%s", i, attr(step, "label") %||% "", err)))
    }
  )
  nxt
}

run_design_internal.default <- function(design) {
  stop("Please only send design objects or functions with no arguments to run_design.")
}

run_design_internal.design <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design), ...) {
  if (!is.list(results)) {
    results <- list(
      estimand = vector("list", length(design)),
      estimator = vector("list", length(design))
    )
  }

  for (i in seq(start, end)) {
    step <- design[[i]]

    causal_type <- attr(step, "causal_type")
    step_type <- attr(step, "step_type")

    # if it's a dgp
    if ("dgp" %in% causal_type) {
      current_df <- next_step(step, current_df, i)
    } else if (step_type %in% names(results)) {
      results[[step_type]][[i]] <- next_step(step, current_df, i)
    } else {
      NULL # skipping steps not in the requested results types
    }
  }

  if (i == length(design)) {
    if ("estimator" %in% names(results)) {
      results[["estimates_df"]] <- rbind_disjoint(results[["estimator"]])
      results[["estimator"]] <- NULL
    }
    if ("estimand" %in% names(results)) {
      results[["estimands_df"]] <- rbind_disjoint(results[["estimand"]])
      results[["estimand"]] <- NULL
    }
    if ("current_df" %in% names(results)) {
      results[["current_df"]] <- current_df
    }
    results
  } else {
    execution_st(
      design = design,
      current_df = current_df,
      results = results,
      start = i + 1,
      end = length(design)
    )
  }
}

# for when the user sends a function that runs a design itself
#   to run_design (or simulate_design / diagnose_design above it)
run_design_internal.function <- function(design) {
  design()
}

run_design_internal.execution_st <- function(design, ...) do.call(run_design_internal.design, design)

# Build an execution strategy object
#
# @param design a design
# @param current_df a data.frame
# @param results a list of intermediate results
# @param start index of starting step
# @param end  index of ending step
execution_st <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design)) {
  # An execution state are the arguments needed to run run_design
  structure(
    list(
      design = design,
      current_df = current_df,
      results = results,
      start = start,
      end = end
    ),
    class = "execution_st"
  )
}


#' @rdname post_design
#'
#' @export
draw_data <- function(design) {
  run_design_internal(design, results = list(current_df = 0))$current_df
}

#' @rdname post_design
#'
#' @export
get_estimands <- function(...) apply_on_design_dots(get_estimands_single_design, ...)

#' @rdname post_design
#'
#' @export
get_estimates <- function(...) apply_on_design_dots(get_estimates_single_design, ...)

apply_on_design_dots <- function(FUN, ...) {
  designs <- dots_to_list_of_designs(...)

  elist <- lapply(designs, FUN)

  if (length(designs) > 1) {
    elist <- Map(cbind, design_label = names(elist), elist, stringsAsFactors = FALSE)
  }

  rbind_disjoint(elist)
}

dots_to_list_of_designs <- function(...) {
  dotqs <- enquos(...)
  d1 <- eval_tidy(dotqs[[1]])

  ## Two cases:
  ## 1. send one or more design objects created by the + operator
  ## 2. send a single list of design objects e.g. created by expand_design
  ## Approach: unpack designs if a list of designs was sent as a single list object
  if (length(dotqs) == 1 &&
    is.list(d1) &&
    !inherits(d1, "design")) {
    designs <- d1
    names(designs) <- infer_names(designs)
  } else {
    names(dotqs) <- infer_names(dotqs)
    designs <- eval_tidy(quo(list(!!!dotqs)))
  }

  # do not allow users to send more than one object if any is not a design object
  check_design_class(designs)

  designs
}


get_estimates_single_design <- function(design) {
  results <- list("estimator" = vector("list", length(design)))
  run_design_internal(design, results = results)$estimates_df
}

get_estimands_single_design <- function(design) {
  results <- list("estimand" = vector("list", length(design)))
  run_design_internal(design, results = results)$estimands_df
}

#' Obtain the preferred citation for a design
#'
#' @param design a design object created using the + operator
#'
#' @param ... options for printing the citation if it is a BibTeX entry
#'
#' @export
cite_design <- function(design, ...) {
  citation <- attr(design, "citation")
  if (class(citation) == "bibentry") {
    print(citation, style = "bibtex", ... = ...)
  } else {
    print(citation, style = "text", ... = ...)
  }
}

#' @export
print.design_step <- function(x, ...) {
  print(attr(x, "call"))
  # invisible(summary(x))
}

#' Print code to recreate a design
#'
#' @param design A design object, typically created using the + operator
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' my_design <- my_population + my_assignment
#'
#' print_code(my_design)
#'
#' @export
print_code <- function(design) {

  # if there is not a code attribute, construct code via the calls for each step
  #   and the call for the declare step

  if (is.null(attributes(design)$code)) {
    clean_call <- function(call) {
      paste(sapply(deparse(call), trimws), collapse = " ")
    }

    # print each step

    for (i in seq_along(design)) {
      # only print steps that are not calls within the design call i.e. mutate(q = 5)
      if (inherits(attributes(design[[i]])$call, "call")) {
        cat(names(design)[i], "<-", clean_call(attributes(design[[i]])$call), "\n\n")
      }
    }

    # print the design declaration

    cat("my_design <-", clean_call(attributes(design)$call), "\n\n")
  } else {
    print(attributes(design)$code)
  }
}

#' @param x a design object, typically created using the + operator
#' @rdname post_design
#' @export
print.design <- function(x, verbose = TRUE, ...) {
  print(summary(x, verbose = verbose, ... = ...))
  # invisible(summary(x))
}

#' @param object a design object created using the + operator
#' @param verbose an indicator for printing a long summary of the design, defaults to \code{TRUE}
#' @param ... optional arguments to be sent to summary function
#'
#' @examples
#'
#' my_population <- declare_population(N = 500, noise = rnorm(N))
#'
#' my_potential_outcomes <- declare_potential_outcomes(
#'   Y_Z_0 = noise, Y_Z_1 = noise +
#'   rnorm(N, mean = 2, sd = 2))
#'
#' my_sampling <- declare_sampling(n = 250)
#'
#' my_assignment <- declare_assignment(m = 25)
#'
#' my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
#'
#' my_mutate <- declare_step(dplyr::mutate, noise_sq = noise ^ 2)
#'
#' my_reveal <- declare_reveal()
#'
#' design <- my_population +
#'   my_potential_outcomes +
#'   my_sampling +
#'   my_estimand +
#'   my_mutate +
#'   my_assignment +
#'   my_reveal +
#'   my_estimator
#'
#' summary(design)
#' @rdname post_design
#' @export
#' @importFrom rlang is_lang
summary.design <- function(object, verbose = TRUE, ...) {
  design <- object

  title <- NULL
  authors <- NULL

  get_formula_from_step <- function(step) {
    call <- attr(step, "call")
    type <- attr(step, "step_type")
    if (is_lang(call) && is.character(type) && type != "wrapped") {
      formulae <- Filter(is_formula, lang_args(call))
      if (length(formulae) == 1) {
        return(formulae[[1]])
      }
    }
    return(NULL)
  }

  variables_added <- variables_modified <-
    quantities_added <- quantities_modified <-
    N <- extra_summary <-
    vector("list", length(design))

  formulae <- lapply(design, get_formula_from_step)
  calls <- lapply(design, attr, "call")

  current_df <- design[[1]]()

  var_desc <- variables_added[[1]] <- lapply(current_df, describe_variable)

  N[[1]] <- paste0("N = ", nrow(current_df))

  estimates_df <- estimands_df <- data.frame()

  last_df <- current_df

  for (i in 1 + seq_along(design[-1])) {
    causal_type <- attr(design[[i]], "causal_type")
    if (is.null(causal_type)) next

    extra_summary[i] <- list(attr(design[[i]], "extra_summary"))

    # if it's a dgp
    if (causal_type == "dgp") {
      current_df <- design[[i]](last_df)

      variables_added_names <-
        get_added_variables(last_df = last_df, current_df = current_df)

      variables_modified_names <-
        get_modified_variables(last_df = last_df, current_df = current_df)

      if (!is.null(variables_added_names)) {
        variables_added[[i]] <-
          lapply(
            current_df[, variables_added_names, drop = FALSE],
            describe_variable
          )
        var_desc[variables_added_names] <- variables_added[[i]]
      }

      if (!is.null(variables_modified_names)) {
        v_mod <- lapply(
          current_df[, variables_modified_names, drop = FALSE],
          describe_variable
        )
        variables_modified[[i]] <- mapply(list,
          before = var_desc[variables_modified_names],
          after = v_mod,
          SIMPLIFY = FALSE, USE.NAMES = TRUE
        )
        var_desc[variables_modified_names] <- v_mod
      }

      N[i] <- local({
        c_row <- nrow(current_df)
        l_row <- nrow(last_df)
        if (c_row == l_row) {
          list(NULL)
        } else {
          sprintf(
            "N = %d (%d %s)", c_row, abs(c_row - l_row),
            ifelse(c_row > l_row, "added", "subtracted")
          )
        }
      })

      last_df <- current_df
    } else if (causal_type %in% c("estimand", "estimator")) {
      quantities_added[[i]] <- design[[i]](current_df)
    }
  }
  
  citation <- attr(design, "citation")
  if (!is.character(citation)) {
    title <- citation$title
    authors <- citation$author
  } 

  function_types <- lapply(design, attr, "step_type")

  structure(
    list(
      variables_added = variables_added,
      quantities_added = quantities_added,
      variables_modified = variables_modified,
      function_types = function_types,
      N = N,
      call = calls,
      formulae = formulae,
      title = title,
      authors = authors,
      citation = citation,
      extra_summary = extra_summary,
      verbose = verbose
    ),
    class = c("summary.design", "list")
  )
}

#' @export
print.summary.design <- function(x, ...) {
  cat("\nDesign Summary\n\n")

  if (!is.null(x$title)) {
    cat("Study title: ", x$title, ifelse(is.null(x$authors), "\n\n", ""), sep = "")
  }

  if (!is.null(x$authors)) {
    cat(
      ifelse(!is.null(x$title), "\n", ""),
      "Authors: ",
      paste0(x$authors, collapse = ", "),
      "\n\n",
      sep = ""
    )
  }

  for (i in 1:max(length(x$variables_added), length(x$quantities_added))) {
    step_name <- if (is.null(x$call[[i]])) "" else deparse(x$call[[i]])
    step_class <-
      ifelse(
        x$function_types[[i]] != "unknown",
        gsub("_", " ", x$function_types[[i]]),
        "custom data modification"
      )

    dash_width <-
      max(c(80 - 11 - nchar(i) - nchar(step_class) - nchar(step_name[1]), 0))

    cat(
      "Step ",
      i,
      " (",
      step_class,
      "): ",
      step_name,
      " ",
      paste0(rep("-", dash_width), collapse = ""),
      "\n\n",
      sep = ""
    )

    if (x$verbose == TRUE) {
      if (!is.null(x$N[[i]])) {
        cat(x$N[[i]], "\n\n")
      }

      if (!is.null(x$formulae[[i]])) {
        cat("Formula:", deparse(x$formula[[i]]), "\n\n")
      }
      if (is.character(x$extra_summary[[i]])) {
        cat(x$extra_summary[[i]], "\n\n")
      }

      if (!is.null(x$quantities_added[[i]])) {
        if (class(x$quantities_added[[i]]) == "data.frame") {
          cat("A single draw of the ", x$function_types[[i]], ":\n", sep = "")
          print(x$quantities_added[[i]], row.names = FALSE)
          cat("\n")
        } else {
          cat(x$quantities_added[[i]], sep = "\n")
          cat("\n")
        }
      }
      if (!is.null(x$variables_added[[i]])) {
        for (j in seq_along(x$variables_added[[i]])) {
          cat("Added variable:", names(x$variables_added[[i]])[j], "\n")
          print(x$variables_added[[i]][[j]], row.names = FALSE)
          cat("\n")
        }
      }
      if (!is.null(x$variables_modified[[i]])) {
        for (j in seq_along(x$variables_modified[[i]])) {
          cat(
            "Altered variable:",
            names(x$variables_modified[[i]])[j],
            "\n  Before: \n"
          )
          print(x$variables_modified[[i]][[j]][["before"]], row.names = FALSE)
          cat("\n  After:\n")
          print(x$variables_modified[[i]][[j]][["after"]], row.names = FALSE)
          cat("\n")
        }
      }
    }
  }

  if (!is.null(x$citation)) {
    cat("Citation:\n")
    print(x$citation)
  }

  invisible(x)
}

#' @export
str.design_step <- function(object, ...) cat("design_step:\t", paste0(deparse(attr(object, "call"), width.cutoff = 500L), collapse = ""), "\n")

# A wrapper around conduct design for fan-out execution strategies
fan_out <- function(design, fan) {
  st <- list(execution_st(design))

  for (i in seq_len(nrow(fan))) {
    end <- fan[i, "end"]
    n <- fan[i, "n"]

    for (j in seq_along(st))
      st[[j]]$end <- end

    st <- st [ rep(seq_along(st), each = n) ]

    st <- future_lapply(seq_along(st), function(j) run_design(st[[j]]), future.seed = NA, future.globals = "st")
  }

  st
}
