


#' @importFrom lazyeval lazy_dots
#' @export
modify_design <- function(design, ...) {
  causal_order <- design$causal_order
  original_env <- design$causal_order_env

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
        causal_order <-
          c(causal_order[1:(location - 1)],
            as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
            causal_order[location:length(causal_order)])
      } else {
        causal_order <-
          c(causal_order[1:(location)],
            as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
            causal_order[(location + 1):length(causal_order)])
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

      causal_order <-
        c(causal_order[1:(location - 1)],
          as.list(dots[[i]]$expr)[2:(length(dots[[i]]$expr) - 1)],
          causal_order[(location + 1):length(causal_order)])

    }

  }

  new_design <- do.call(what = declare_design,
                        args = causal_order,
                        envir = original_env)
  new_design
}

#' @importFrom lazyeval lazy_dots
#' @export
add_step <- function(..., before = NULL, after = NULL) {
  obj <- list(
    additions = lazy_dots(...),
    before = lazy_dots(before),
    after = lazy_dots(after)
  )
  return(obj)
}

#' @importFrom lazyeval lazy_dots
#' @export
remove_step <- function(...) {
  obj <- list(removals = lazy_dots(...))
  return(obj)
}

#' @importFrom lazyeval lazy_dots
#' @export
replace_step <- function(..., replace) {
  obj <- list(replacements = lazy_dots(...),
              replace = lazy_dots(replace))
  return(obj)
}
