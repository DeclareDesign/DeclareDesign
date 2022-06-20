

#' @param x a design object, typically created using the + operator
#' @rdname post_design
#' @export
print.design <- function(x, verbose = FALSE, ...) {
  print(summary(x, verbose = verbose, ... = ...))
  
  cat("Run of the design:\n\n")
  run <- try({run_design(x)})
  if(!inherits(run, "try-error")) {
    print(run_design(x), digits = 3, row.names = FALSE)
  }
  cat("\n")
}

#' @param object a design object created using the + operator
#' @param verbose an indicator for printing a long summary of the design, defaults to \code{TRUE}
#' @param ... optional arguments to be sent to summary function
#'
#' @examples
#'
#' my_model <- 
#'   declare_model(
#'     N = 500, 
#'     noise = rnorm(N),
#'     Y_Z_0 = noise, 
#'     Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
#'   )
#'
#' my_sampling <- declare_sampling(S = complete_rs(N, n = 250))
#'
#' my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
#'
#' my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#'
#' my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
#'
#' my_reveal <- declare_measurement(Y = reveal_outcomes(Y ~ Z))
#'
#' design <- my_model +
#'   my_sampling +
#'   my_inquiry +
#'   my_assignment +
#'   my_reveal +
#'   my_estimator
#'
#' summary(design)
#' @rdname post_design
#' @export
#' @importFrom rlang is_call call_args
summary.design <- function(object, verbose = TRUE, ...) {
  design <- object
  
  title <- NULL
  authors <- NULL
  
  get_formula_from_step <- function(step) {
    call <- attr(step, "call")
    type <- attr(step, "step_type")
    if (is_call(call) && is.character(type) && type != "wrapped") {
      formulae <- Filter(is_formula, call_args(call))
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
  
  estimates_df <- inquiries_df <- data.frame()
  
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
    } else if (causal_type %in% c("inquiry", "estimator")) {
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

chr_squish <- function(txt) {
  txt <- paste0(txt, collapse = " ")
  gsub("^ *|(?<= ) | *$", "", txt, perl = TRUE)
}

#' @export
print.summary.design <- function(x, ...) {
  cat("\nResearch design declaration summary\n\n")
  
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
    step_name <- if (is.null(x$call[[i]])) "" else chr_squish(deparse(x$call[[i]]))
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
        cat("Formula:", chr_squish(deparse(x$formula[[i]])), "\n\n")
      }
      if (is.character(x$extra_summary[[i]])) {
        cat(x$extra_summary[[i]], "\n\n")
      }
      
      if (!is.null(x$quantities_added[[i]])) {
        if (inherits(x$quantities_added[[i]], "data.frame")) {
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
