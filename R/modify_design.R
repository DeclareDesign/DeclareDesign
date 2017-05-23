


#' Modify a Design
#' @param design a design object, usually created by \code{\link{declare_design}}, \code{\link{quick_design}}, or \code{\link{download_design}}.
#'
#' @param ... a series of calls to \code{\link{add_step}}, \code{\link{remove_step}}, or \code{\link{replace_step}}
#'
#' @return A design object. See \code{\link{declare_design}} for details.
#'
#' @importFrom lazyeval lazy_dots
#' @export
#'
#' @examples
#'
#'  my_population <- declare_population(N = 100, noise = rnorm(N))
#'
#'  my_potential_outcomes <-
#'    declare_potential_outcomes(Y_Z_0 = noise,
#'                               Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
#'
#'  my_assignment <- declare_assignment(m = 50)
#'  my_assignment_2 <- declare_assignment(m = 25)
#'
#'  design <- declare_design(my_population,
#'                           my_potential_outcomes,
#'                           my_assignment)
#'
#'  design
#'
#'  modify_design(design, replace_step(my_assignment_2, replace = my_assignment))
#'
#'  modify_design(design, add_step(dplyr::mutate(income = noise^2), after = my_assignment))
#'  modify_design(design, add_step(dplyr::mutate(income = noise^2), before = my_assignment))
#'
#'  modify_design(design, remove_step(my_assignment))
modify_design <- function(design, ...) {
  causal_order <- design$causal_order
  original_env <- design$causal_order_env

  # modify_env <- freeze_environment(parent.frame())

  # overlap_names <-
  #   ls(modify_env)[ls(modify_env) %in% ls(original_env)]
  #
  # ## show warning if there is any overlap in objects, say they will NOT be replaced
  # if (length(overlap_names) > 0) {
  #   warning(
  #     paste0(
  #       "Note that some of the objects in your global environment currently were also used in your original design. The version of ",
  #       paste(overlap_names, collapse = ", "),
  #       " created before declare_design will be used."
  #     )
  #   )
  # }
  #
  # ## add any parts of modify_env to original_env that are NOT in original env
  #
  # ##for(ls(modify_env)[!(ls(modify_env) %in% overlap_names)])
  #
  # ## for(n in ls(e1, all.names = TRUE)) assign(n, get(n, e1), e2)
  #

  dots <- lazy_dots(...)
  dots_funcs <- sapply(dots, function(x)
    deparse(x$expr[[1]]))

  for (i in seq_along(dots)) {


    ## add step
    if (dots_funcs[i] ==  "add_step") {
      step_names <- names(dots[[i]]$expr)
      location <-
        which(causal_order == dots[[i]]$expr[[length(step_names)]])
      before <- step_names[length(step_names)] == "before"

      if (before) {

        if (location == 1) {
          causal_order <-
            c(as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
              causal_order)
        }else{
          causal_order <-
            c(causal_order[1:(location - 1)],
              as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
              causal_order[location:length(causal_order)])
        }

      } else {
        if(location == length(causal_order)){
          causal_order <-
            c(causal_order,
              as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)])

        }else{
          causal_order <-
            c(causal_order[1:(location)],
              as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
              causal_order[(location + 1):length(causal_order)])
        }

      }
    }

    ## remove step

    if (dots_funcs[i] == "remove_step") {
      location <-
        which(causal_order == dots[[i]]$expr[[2]])
      causal_order[[location]] <- NULL
    }

    ## replace step

    if (dots_funcs[i] == "replace_step") {
      location <-
        which(causal_order == dots[[i]]$expr[[length(dots[[i]]$expr)]])

      if (location == 1) {
        causal_order <-
          c(as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
            causal_order[(location + 1):length(causal_order)])
      } else if (location == length(causal_order)) {
        causal_order <-
          c(causal_order[1:(location - 1)],
            as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)])
      } else{

        causal_order <-
          c(causal_order[1:(location - 1)],
            as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
            causal_order[(location + 1):length(causal_order)])

      }

    }
  }
  new_design <- do.call(what = declare_design,
                        args = causal_order,
                        envir = original_env)
  new_design
}

#' Modify a Design by Adding Steps
#' @param ... steps to add to a design
#'
#' @param before bare (unquoted) name of the step before which to add steps.
#' @param after bare (unquoted) name of the step after which to add steps.
#'
#' @importFrom lazyeval lazy_dots
#' @details see \code{\link{modify_design}} for details.
#'
#' @export
add_step <- function(..., before = NULL, after = NULL) {
  obj <- list(
    additions = lazy_dots(...),
    before = lazy_dots(before),
    after = lazy_dots(after)
  )
  return(obj)
}

#' Modify a Design by Removing Steps
#' @param ... bare (unquoted) names of steps to remove
#'
#' @importFrom lazyeval lazy_dots
#'
#' @details see \code{\link{modify_design}} for details.
#' @export
remove_step <- function(...) {
  obj <- list(removals = lazy_dots(...))
  return(obj)
}

#' Modify a Design by Replacing a Step
#' @param ... replacement step(s)
#'
#' @param replace step to be replaced
#'
#' @importFrom lazyeval lazy_dots
#'
#' @details see \code{\link{modify_design}} for details.
#'
#' @export
replace_step <- function(..., replace) {
  obj <- list(replacements = lazy_dots(...),
              replace = lazy_dots(replace))
  return(obj)
}
