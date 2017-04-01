

# @importFrom rlang SOMETHING
#' @importFrom DDfabricate fabricate_data
#' @importFrom lazyeval lazy_dots make_call lazy_eval
#' @export
declare_test <- function(func = default_test_function, ...) {

  ## this creates a formula, quoting the function func with UQ(func)
  ## then creating the arguments to the function using c() which
  ## concatenates two parts, first the arguments sent to ...
  ## through dots_quos(...) and second adding an argument data = data,
  ## which doesn't yet exist (it's just a promise) and will exist
  ## inside internal_function
  internal_call <- dots_quos(UQ(func)(!!!c(dots_quos(...), quos(data = data))))

  internal_function <- function(data){
    ## tidy eval the call and all arguments come from the environment
    ## of form, except data which we set directly through the data = list()
    ## argument. We send it data coming into internal_function.
    eval_tidy(internal_call, data = list(data = data))
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
