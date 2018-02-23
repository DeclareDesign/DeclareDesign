
#' @importFrom rlang env_clone
env_deep_copy <- function(e) {
  if(identical(e, emptyenv())) emptyenv() else
    if(identical(e, globalenv())) env_clone(e) else # don't clone attached packages
      env_clone(e, Recall(parent.env(e)))
}

# Given a function and dots, rename dots based on how things will positionally match
#' @importFrom rlang is_empty
rename_dots <- function(handler, dots, addData=TRUE){
  if(is_empty(dots)) return(dots)
  f <- function(...) as.list(match.call(handler)[-1])

  d_idx <- setNames(seq_along(dots), names(dots))

  if(addData) d_idx["data"] <- list(NULL)

  d_idx <- eval_tidy(quo(f(!!!d_idx)))

  if(addData) d_idx$data <- NULL

  d_idx <- unlist(d_idx)
  # browser()
  d_idx <- d_idx[names(d_idx) != "" ]

  names(dots)[d_idx] <- names(d_idx)

  dots
}


currydata <- function(FUN, dots, addDataArg=TRUE,strictDataParam=TRUE) {
  # dots <- quos(...)

  #  for(i in seq_along(dots)) {
  #    environment(dots[[i]]) <- env_clone(dots[[i]])

  # heuristic to reuse deep clones
  eprev <- NULL
  for(i in seq_along(dots)) {
    ecurrent <- environment(dots[[i]])
    if(!is.null(ecurrent)) {
      if(!identical(ecurrent, eprev) ) {
        eprev <- ecurrent
        eclone <- env_deep_copy(ecurrent)
      }
      environment(dots[[i]]) <- eclone
    }
  }

  quoNoData <- quo((FUN)(!!!dots))

  if(addDataArg && !'data' %in% names(dots) && !'.data' %in% names(dots)){

    dots <- append(dots, list(data=quote(data)), after=FALSE)
  }

  quo <- quo((FUN)(!!!dots))

  if(isTRUE(strictDataParam)) function(data) eval_tidy(quo, data=list(data=data))
  else  function(data=NULL){
    #message(quo)
    #used for declare_population with no seed data provided
    res <- if(is.null(data)) eval_tidy(quoNoData) else eval_tidy(quo, data=list(data=data))
    res
  }
}

#' @importFrom rlang enquo
declaration_template <- function(..., handler, label=NULL){
  #message("Declared")

  dots <- quos(...,label=!!label)
  this <- attributes(sys.function())

  if(!"label" %in% names(formals(handler))){
    dots$label <- NULL
  }

  dots <- rename_dots(handler, dots, this$strictDataParam)


  ret <- build_step(currydata(handler, dots, strictDataParam=this$strictDataParam),
            handler=handler,
            dots=dots,
            label=label,
            step_type=this$step_type,
            causal_type=this$causal_type,
            call=match.call())

  if(has_validation_fn(handler)) ret <- validate(handler, ret,  dots, label)

  ret
}

build_step <- function(curried_fn, handler, dots, label, step_type, causal_type, call){
  structure(curried_fn,
            handler=handler,
            dots=dots,
            label=label,
            step_type=step_type,
            causal_type=causal_type,
            call=call,
            class=c("design_step", "function"))
}

make_declarations <- function(default_handler, step_type, causal_type='dgp', default_label, strictDataParam=TRUE) {

  declaration <- declaration_template


  formals(declaration)$handler <- substitute(default_handler)
  if(!missing(default_label)) formals(declaration)$label <- default_label

  structure(declaration,
            class=c('declaration', 'function'),
            step_type=step_type,
            causal_type=causal_type,
            strictDataParam=strictDataParam)
}

###############################################################################

validation_fn <- function(f){
  attr(f, "validation_fn")
}

`validation_fn<-` <- with_validation_fn <- function(x, value) {
  attr(x, "validation_fn") <- value
  x
}

has_validation_fn <- function(f){
  is.function(validation_fn(f))
}

validate <- function(handler, ret, dots, label) {
  validation_fn(handler)(ret, dots, label)
}


###############################################################################

#' @param ...      arguments to be captured, and later passed to the handler
#' @param handler  a tidy-in, tidy-out function
#' @param label    a string describing the step
#' @keywords internal
declare_internal_inherit_params <- make_declarations(function(data, ...) data.frame(HIA="RYLAH"), step_type = "HIARYLAH")
