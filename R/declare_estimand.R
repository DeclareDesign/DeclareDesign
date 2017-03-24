

#' @export
declare_estimand <- function(..., estimand_function = default_estimand_function, label = "my_estimand") {

  declare_estimand_(paste(substitute(alist(...)))[-1],
                    estimand_function = estimand_function,
                    label = deparse(substitute(label)))

}

#' @export
declare_estimand_ <- function(..., estimand_function = default_estimand_function, label = "my_estimand"){

  estimand_options <- list(...)

  estimand_function_internal <- function(data){
    estimand_options$data <- data
    value <- do.call(estimand_function, args = estimand_options)
    return(data.frame(estimand_label = label, estimand = value, stringsAsFactors = FALSE))
  }

  attributes(estimand_function_internal) <- list(call = match.call(), type = "estimand", label = label)

  return(estimand_function_internal)
}


#' @export
default_estimand_function <- function(data, estimand){
  estimand <- parse(text = estimand)
  data_environment <- list2env(data)
  eval(estimand, envir = data_environment)
}
