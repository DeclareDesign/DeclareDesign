



#' @importFrom rlang f_text f_env

maybe_add_labels <- function(quotations){

  labeller <- function(quotation, lbl) {
    cx <- quotation[[2]]

    if (lbl == "") {
      lbl <- f_text(quotation)
    }

    if(is.call(cx) && is.symbol(cx[[1]]) && ! "label" %in% names(cx)){

      f <- get0(as.character(cx[[1]]), f_env(quotation), "function") #match.fun does not repect quosures environment, doing get manually
      if("declaration" %in% class(f) && "label" %in% names(formals(f))){
        quotation[[2]][["label"]] <- lbl
      }
    }
    quotation
  }

  for(i in seq_along(quotations)){
    quotations[[i]] <- labeller(quotations[[i]], names(quotations)[i])
  }

  quotations
}

declare_time_error <- function(message, declaration){
  stop( simpleError(message, call = attr(declaration, "call")) )
}

declare_time_warn <- function(message, declaration){
  stop( simpleWarning(message, call = attr(declaration, "call")) )
}


future_lapply <- function(..., future.seed = NA, future.globals=TRUE){
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(..., future.seed=future.seed, future.globals = future.globals)
  } else {
    lapply(...)
  }
}


# If <= 5 uniques, table it, ow descriptives if numeric-ish, ow number of levels.
describe_variable <- function(x) {

  num_unique=length(unique(x))

  if(num_unique > 5) {
    return(describe_variable_impl(x, num_unique))
  }

  tab <- table(x, exclude = NULL)

  rbind.data.frame(
    Frequency=data.frame(as.list(tab), check.names = FALSE),
    Proportion=sprintf("%.2f", prop.table(tab))
  )

}

describe_variable_impl <- function(x, num_unique) UseMethod("describe_variable_impl")

describe_variable_impl.factor <- function(x, num_unique) {
  data.frame(
    as.list(summary.factor(x, 5)),
    N_unique=num_unique,
    check.names = FALSE
  )
}

describe_variable_impl.character <-  function(x, num_unique) {
  data.frame(
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    class = class(x),
    stringsAsFactors = FALSE
  )
}

#' @importFrom stats median sd
describe_variable_impl.default <- function(x, num_unique) {
  #todo just use summary?
  data.frame(
    lapply(
      list(min=min, median=median, mean=mean, max=max, sd=sd),
      function(f,x) round(f(x, na.rm=TRUE), digits=2),
      x
    ),
    N_missing = sum(is.na(x)),
    N_unique = num_unique,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


`%i%` <- intersect

`%||%` <- function(e1, e2) if(is.null(e1)) e2 else e1
