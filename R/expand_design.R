#' Declare a design via a designer
#'
#' \code{expand_design} easily generates a set of design from a designer function.
#'

#' @param designer a function which yields a design
#' @param ... Options sent to the designer
#' @param expand boolean - if true, form the crossproduct of the ..., otherwise recycle them
#' @param prefix prefix for the names of the designs, i.e. if you create two designs they would be named prefix_1, prefix_2
#'
#' @return if set of designs is size one, the design, otherwise a `by`-list of designs. Designs are given a parameters attribute with the values of parameters assigned by expand_design.
#'
#' @examples
#'
#' designer <- function(N) {
#'   pop <- declare_population(N = N, noise = rnorm(N))
#'   pos <- declare_potential_outcomes(Y ~ 0.20 * Z + noise)
#'   assgn <- declare_assignment(m = N / 2)
#'   mand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#'   mator <- declare_estimator(Y ~ Z, estimand = mand)
#'   pop + pos + assgn + mand + mator
#' }
#'
#' # returns list of eight designs
#' designs <- expand_design(designer, N = seq(30, 100, 10))
#'
#' \dontrun{
#'  # diagnose a list of designs created by expand_design or redesign
#'  diagnosis <- diagnose_design(designs, sims = 50)
#' }
#'
#' # returns a single design
#' large_design <- expand_design(designer, N = 200)
#'
#' \dontrun{
#'  diagnose_large_design <- diagnose_design(large_design, sims = 50)
#' }
#'
#' @export
expand_design <- function(designer, ..., expand = TRUE, prefix = "design") {
  dots_quos <- quos(...)

  if (length(dots_quos) > 0) {
    args_list <- expand_args(..., expand = expand)
    args_names <- expand_args_names(!!!dots_quos, expand = expand)
    # args_names <- rbind_disjoint(lapply(args_list, data.frame))

    designs <- lapply(args_list, function(x) do.call(designer, args = x))

    for (i in seq_along(designs)) {
      attr(designs[[i]], "parameters") <- setNames(args_names[i, , drop = FALSE], names(args_names))
    }

    if (length(designs) == 1) {
      designs <- designs[[1]]
    } else {
      names(designs) <- paste0(prefix, "_", seq_len(length(designs)))
    }
  } else {
    designs <- designer()
  }

  return(designs)
}


expand_args <- function(..., expand = TRUE) {
  dots <- list(...)

  if (expand) {
    lens <- lapply(dots, function(x) seq_len(length(x)))
    args_positions <- do.call(expand.grid, args = list(lens, stringsAsFactors = TRUE))
    args_list <- vector("list", nrow(args_positions))
    for (i in seq_len(nrow(args_positions))) {
      current_list_row <- vector("list", ncol(args_positions))
      names(current_list_row) <- names(dots)
      for (j in seq_len(ncol(args_positions))) {
        if (length(dots[[j]]) > 1) {
          current_list_row[[j]] <- dots[[j]][[args_positions[i, j]]]
        } else {
          current_list_row[[j]] <- dots[[j]]
        }
      }
      args_list[[i]] <- current_list_row
    }
  } else {
    args_list <- vector("list", length(dots[[1]]))
    for (i in seq_along(args_list)) {
      current_list_row <- vector("list", length(dots))
      names(current_list_row) <- names(dots)
      for (j in seq_along(dots)) {
        if (length(dots[[j]]) > 1) {
          current_list_row[[j]] <- dots[[j]][j]
        } else {
          current_list_row[[j]] <- dots[[j]]
        }
      }
      args_list[[i]] <- current_list_row
    }
  }
  args_list
}

#' @importFrom rlang quo_squash is_call call_args
expand_args_names <- function(..., expand = TRUE) {
  dots_quos <- quos(...)

  dots_names <- lapply(dots_quos, function(x) {
    x_expr <- quo_squash(x)
    is_list_c <- expr_text(as.list(x_expr)[[1]]) %in% c("c", "list")
    if (!is_list_c) {
      x_is_call <- is_call(x_expr)
      if (x_is_call) {
        as.character(eval_tidy(x))
      } else {
        as.character(x_expr)
      }
    } else {
      as.character(call_args(x_expr))
    }
  })
  if (expand) {
    ret <- expand.grid(dots_names, stringsAsFactors = FALSE)
  } else {
    ret <- data.frame(dots_names, stringsAsFactors = FALSE)
  }
  ret
}


