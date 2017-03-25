


#' @export
modify_design <- function(design, ...) {
  from_to_list <- eval(substitute(alist(...)))
  from_to_list_names <- paste(from_to_list)

  ## change so it pulls out the expr from the lazy expression
  ## then recreates but retains the environment

  # it has two arguments and neither are tofrom, overwrite from_to_list
  if (length(from_to_list_names) == 2 &
      !any(startsWith(from_to_list_names, "from_to("))) {
    from_to_list <- list(do.call(from_to, args = from_to_list))
  } else if (all(startsWith(from_to_list_names, "from_to("))) {
    from_to_list <- lapply(from_to_list, eval)
  }

  design_call <- design$call
  design_call_names <- paste(design_call)

  for (i in 1:length(from_to_list)) {
    design_call[[which(design_call_names == from_to_list[[i]]["from"])]] <-
      from_to_list[[i]]["to"]$to

  }
  return(eval(design_call))
}

# this is a helper function that allows the syntax
# modify_design(design, from_to(sate, pate))
# to work
#' @export
from_to <- function(from, to) {
  return(list(from = substitute(from), to = substitute(to)))
}
