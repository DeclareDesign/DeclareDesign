from_package <- function(func, package){
  func_package <- tryCatch(getNamespaceName(environment(func)), error = function(e) NULL)
  ifelse(is.null(func_package), FALSE, func_package == package)
}



## declare_estimand(ATE = mean(Y_Z_1-Y_Z_0)) ## ATE

#' @importFrom lazyeval lazy_dots make_call lazy_eval call_modify
#' @export
declare_estimand <- function(..., label = NULL, estimand_function = default_estimand_function) {
  args <- eval(substitute(alist(...)))
  env <- freeze_environment(parent.frame())
  func <- eval(estimand_function)

  ## handles four uses cases of labeling so it's really easy to label without specifying
  ## would be great to clean up the next 10 lines
  if (substitute(estimand_function) == "default_estimand_function" &
      from_package(estimand_function, "DeclareDesign")) {
    label_internal <- names(args)[1]
  }

  if(!exists("label_internal") | is.null(label_internal)){
    label_internal <- substitute(label)
    if(!is.null(label_internal)){
      label_internal <- as.character(label_internal)
    } else {
      label_internal <- "my_estimand"
    }
  }

  estimand_function_internal <- function(data) {
    args$data <- data
    value <- do.call(func, args = args, envir = env)
    data.frame(estimand_label = label_internal, estimand = value, stringsAsFactors = FALSE)
  }
  attributes(estimand_function_internal) <-
    list(call = match.call(), type = "estimand", label = label_internal)

  return(estimand_function_internal)
}

#' @importFrom lazyeval lazy_eval lazy_dots
#' @export
default_estimand_function <- function(data, ...){
  options <- lazy_dots(...)
  if(length(options) > 1){
    stop("Please only provide a single estimand to declare_estimand.")
  }

  lazy_eval(options[[1]], data = data)
}
