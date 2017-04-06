# library(DeclareDesign)
# library(fabricatr)
# library(magrittr)
# library(dplyr)
#
# pop <- declare_population(N =4, X = rbeta(N, 2, 3))
# declare_design(pop()) %>% draw_data()
#
# dumb_dumb <- function(data){
#   return(data)
# }
#
# declare_design(pop(), dumb_dumb) %>% draw_data()
#
#
# # WHAT IS HAPPENING HERE?
# dumb_mutate <- function(data, ...){
#   data <- tibble::as_tibble(x = data)
#   data <- mutate(.data = data, ... = ...)
#   return(data)
# }
#
# pop() %>%  dumb_mutate(me = 5)
# ##debugonce(declare_design)
# declare_design(pop(), dumb_mutate(me = 5)) %>% draw_data()
#
#
#
# # OKOKOK, CRAZY gremlins.
# # This worked for a while, then I debugged in and didn't change anything then it stopped working
#
# declare_design(pop(), dplyr::mutate_(~ X*2)) %>% draw_data()
# declare_design(pop(), dplyr::mutate(Q = X*2)) %>% draw_data()
#
# declare_design(pop(), dplyr::as_data_frame, dplyr::mutate_(~ X*2)) %>% draw_data()
# declare_design(pop(), dplyr::as_data_frame, dplyr::mutate(Q = X*2)) %>% draw_data()
#
# declare_design(pop(), dumb_mutate, dplyr::mutate_(~ X*2)) %>% draw_data()
# declare_design(pop(), dumb_mutate, dplyr::mutate(Q = X*2)) %>% draw_data()
#
# declare_design(pop(), as.data.frame, dplyr::mutate_(~ X*2)) %>% draw_data()
# declare_design(pop(), as.data.frame, dplyr::mutate(Q = X*2)) %>% draw_data()
#
