#' Declare a design 
#'
#' @param lhs A step in a research design, beginning with a function that defines the model. Steps are evaluated sequentially. With the exception of the first step, all steps must be functions that take a \code{data.frame} as an argument and return a \code{data.frame}. Steps are declared using the \code{declare_} functions, i.e., \code{\link{declare_model}}, \code{\link{declare_inquiry}}, \code{\link{declare_sampling}}, \code{\link{declare_assignment}}, \code{\link{declare_measurement}}, \code{\link{declare_estimator}}, and \code{\link{declare_test}}.
#' @param rhs A second step in a research design
#'
#' @return a design
#'
#' @name declare_design
#'
#' @importFrom rlang quos eval_tidy quo_text is_formula is_symbol
#' @importFrom utils bibentry
#' @export
#'
#' @examples
#'
#' design <-
#'   declare_model(
#'     N = 500, 
#'     U = rnorm(N),
#'     potential_outcomes(Y ~ Z + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N, n = 250)) +
#'   declare_assignment(Z = complete_ra(N, m = 25)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE") 
#' 
#' dat <- draw_data(design)
#' head(dat)
#' 
#' run_design(design)
#' 
#' # You may wish to have a design with only one step:
#' 
#' design <- declare_model(N = 500, noise = rnorm(N)) + NULL
#' 
#' dat <- draw_data(design)
#' head(dat)
#'
#' @importFrom rlang enexpr expr_deparse is_null is_missing enquos f_rhs
#'
#' @export
`+.dd` <- function(lhs, rhs) {

  # two cases
  # 1. lhs is a step
  # 2. lhs is a design

  if (missing(rhs)) {
    rhs <- NULL
    qs <- enquos(lhs)
  } else {
    qs <- enquos(lhs, rhs)
  }

  if (!inherits(rhs, "dd") && !inherits(rhs, "function") && !is.null(rhs)) {
    stop("The right hand side of the + does not appear to be a ",
      "DeclareDesign object or a function.",
      call. = FALSE
    )
  }

  lhs <- if (inherits(lhs, "design")) {
    lhs
  } else {
    wrap_step(lhs, f_rhs(qs[[1]]))
  }

  rhs <- if (inherits(rhs, "design")) {
    rhs
  } else if (!is.null(rhs)) wrap_step(rhs, f_rhs(qs[[2]]))

  unique_nms <- make.unique(c(names(lhs), names(rhs)), sep = "_")

  if (!is.null(rhs)) {
    names(rhs) <- unique_nms[(length(lhs) + 1):length(unique_nms)]
  }

  steps <- append(lhs, rhs)

  construct_design(steps)
}


construct_design <- function(steps) {
  ret <- structure(steps,
    call = match.call(),
    class = c("design", "dd")
  )

  # for each step in qs, eval, and handle edge cases (dplyr calls, non-declared functions)
  for (i in seq_along(ret)) {


    # Is it a non-declared function
    if (is.function(ret[[i]]) && !inherits(ret[[i]], "design_step")) {

      # warn if the function call does not have exactly data as arguments
      #  except: if it is a dplyr pipeline (class fseq)
      if (!identical(names(formals(ret[[i]])), "data") &&
        !inherits(ret[[i]], "fseq")) {
        warning("Undeclared Step ", i, " function arguments are not exactly 'data'")
      }

      ret[[i]] <- build_step(
        ret[[i]],
        handler = NULL, dots = list(), label = names(ret)[i],
        step_type = "undeclared", causal_type = "dgp", call = attr(ret[[i]], "call")
      )
    }
  }

  # If there is a design-time validation, trigger it
  for (i in seq_along(ret)) {
    step <- ret[[i]]
    callback <- attr(step, "design_validation")
    if (is.function(callback)) {
      ret <- callback(ret, i, step)
    }
  }

  # ensure all names are unique
  unique_nms <- make.unique(names(ret), sep = "_")

  # Assert that all labels are unique
  local({
    labels <- sapply(ret, attr, "label")
    function_types <- sapply(ret, attr, "step_type")

    check_unique_labels <- function(labels, types, what) {
      ss <- labels[types == what]
      if (anyDuplicated(ss)) {
        what <- ifelse(what == "inquiry", "inquirie", what)
        stop(
          "You have ", what, "s with identical labels: ",
          unique(ss[duplicated(ss)]),
          "\nPlease provide ", what, "s with unique labels"
        )
      }
    }

    check_unique_labels(labels, function_types, "inquiry")
    check_unique_labels(labels, function_types, "estimator")
  })

  ret
}
