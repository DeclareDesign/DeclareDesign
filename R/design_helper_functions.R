#' Explore your design
#'
#' @param design A design created by \code{\link{declare_design}}.
#'
#' @examples
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_sampling,
#'                          my_estimand,
#'                          dplyr::mutate(noise_sq = noise^2),
#'                          my_assignment,
#'                          reveal_outcomes,
#'                          my_estimator)
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

#' @rdname post_design
#'
#' @export
draw_data <- function(design) {
  current_df <- NULL

  for(step in design) {
    if("dgp" %in% attr(step, "causal_type"))
      current_df <- step(current_df)
  }

  current_df
}

#' @export
execute_design <- function(design) {
  current_df <- NULL

  results <- list(estimand=vector("list", length(design)),
                  estimator=vector("list", length(design)))

  for (i in seq_along(design)) {
    step <- design[[i]]
    df <- step(current_df)

    # if it's a dgp
    if ("dgp" %in% attr(step, "causal_type")) {
      current_df <- df
    } else {
      results[[attr(step, "step_type")]][[i]] <- df
    }
  }
  list(estimates_df = do.call(rbind.data.frame, results[["estimator"]]),
       estimands_df = do.call(rbind.data.frame, results[["estimand"]]))
}




#' @rdname post_design
#'
#' @export
get_estimates <- function(design) {
  execute_design(design)$estimates_df
}

#' @rdname post_design
#'
#' @export
get_estimands <- function(design) {
  execute_design(design)$estimands_df
}

#' Obtain the preferred citation for a design
#'
#' @param design a design object created by \code{declare_design}
#'
#' @param ... options for printing the citation if it is a BibTeX entry
#'
#' @export
cite_design <- function(design, ...) {
  citation <- Filter(function(step) attr(step, "causal_type") == 'citation', design)[[1]]()
  if (class(citation) == "bibentry") {
    print(citation, style = "bibtex", ... = ...)
    cat("\n")
  }
  print(citation, style = "text", ... = ...)
  invisible(design)
}

#' @export
print.design <- function(x, ...) {
  print(summary(x))
  # invisible(summary(x))
}

#' Text Summary of a Design
#'
#' @param object a design object created by \code{\link{declare_design}}
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
#' design <- declare_design(my_population,
#'                          my_potential_outcomes,
#'                          my_sampling,
#'                          my_estimand,
#'                          dplyr::mutate(noise_sq = noise^2),
#'                          my_assignment,
#'                          reveal_outcomes,
#'                          my_estimator)
#'
#' summary(design)
#' @export
summary.design <- function(object, ...) {
  summary_function(object)
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

  if (!is.null(x$description)) {
    cat(x$description, "\n\n")
  }

  for (i in 1:max(length(x$variables_added), length(x$quantities_added))) {
    step_name <- deparse(x$call[[i]])
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

    if (!is.null(x$N[[i]])) {
      cat(x$N[[i]], "\n\n")
    }

    if (!is.null(x$formulae[[i]])) {
      cat("Formula:", deparse(x$formula[[i]]), "\n\n")
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
        cat("Altered variable:",
            names(x$variables_modified[[i]])[j],
            "\n  Before: \n")
        print(x$variables_modified[[i]][[j]][["before"]], row.names = FALSE)
        cat("\n  After:\n")
        print(x$variables_modified[[i]][[j]][["after"]], row.names = FALSE)
        cat("\n")
      }
    }
  }

  if (!is.null(x$citation)) {
    cat("Citation:\n")
    print(x$citation)
  }

  invisible(x)
}
