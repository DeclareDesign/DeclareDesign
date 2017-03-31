

# @importFrom rlang SOMETHING
#' @importFrom DDfabricate fabricate_data
#' @importFrom lazyeval lazy_dots make_call lazy_eval
#' @export
declare_test <- function(func = default_test_function, ...) {

  ##func_quo <- quo(func)
  ##dots <- dots_quos(...)

  ## make a call
  ##form <- dots_quos(UQ(func)(!!! dots_quos(...)))

  ##dots <- c(dots_quos(...), quos(data = data))

  form <- dots_quos(UQ(func)(!!! c(dots_quos(...), quos(data = data))))

  internal_function <- function(data){
    ## tidy eval the call that combines func and dots
    eval_tidy(form, data = list(data = data))
  }
  ## attributes
  ## return function

  internal_function

}


default_test_function <- function(data){
  data %>% mutate(q = "default")
}


# MWE
#
# library(dplyr)
#
# start_data <- data.frame(start = rep("my_start", 10))
#
# my_function <- function(data, my_value){
#   data[, "my_value"] <- my_value
#   data
# }
#
# ##debugonce(declare_test)
# test <- declare_test(func = my_function, my_value = 5)
#
# rm(my_function)
# ##debugonce(test)
# test(start_data)
