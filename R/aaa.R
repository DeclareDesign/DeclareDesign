
#' @importFrom rlang env_clone
env_deep_copy <- function(e) {
  if(identical(e, emptyenv())) emptyenv() else
    if(identical(e, globalenv())) env_clone(e) else # don't clone attached packages
      env_clone(e, Recall(parent.env(e)))
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

default_declaration_validation_callback <- function(decl, dots) decl

#' @importFrom rlang enquo
declaration_template <- function(..., handler, label=NULL){
  #message("Declared")
  d <- enquo(handler);

  dots <- quos(...,label=!!label)
  this <- attributes(sys.function())

  if(!"label" %in% names(formals(handler))){
    dots$label <- NULL
  }




  ret <- structure(currydata(handler, dots, strictDataParam=this$strictDataParam),
            label=label,
            step_type=this$step_type,
            causal_type=this$causal_type,
            call=match.call() )

  if(is.function(this$validation)) ret <- this$validation(ret, handler, dots, label)

  ret
}

make_declarations <- function(default_handler, step_type, causal_type='dgp', default_label, validation=NULL, strictDataParam=TRUE) {

  declaration <- declaration_template


  formals(declaration)$handler <- substitute(default_handler)
  if(!missing(default_label)) formals(declaration)$label <- default_label

  structure(declaration,
            class=c('Declared', 'function'),
            step_type=step_type,
            causal_type=causal_type,
            validation=validation,
            strictDataParam=strictDataParam)
}





