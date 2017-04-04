
library(DDfabricate)
library(DeclareDesign)
library(magrittr)
library(dplyr)

# set up a population

my_population_function <- function(N) {
  data.frame(u = rnorm(N))
}

my_population_custom <- declare_population(
  population_function = my_population_function, N = 100)

rm(my_population_function)
pop_custom <- my_population_custom()

# draw POs for it without arguments

my_po_function <- function(data) {
  data %>% mutate(Y_Z_0 = .25 + u, Y_Z_1 = u)
}

my_po_custom <- declare_potential_outcomes(
  potential_outcomes_function = my_po_function)

pop_custom <- my_po_custom(pop_custom)


my_estimand <- declare_estimand(mean(Y_Z_1 - Y_Z_0))

my_estimand(pop_custom)

# do quick design

design_func <- function(numb){
  pop <- declare_population(N = numb, u = rnorm(N))
  my_po_function_qd <- function(data) {
    data %>% mutate(Y_Z_0 = .25 + u, Y_Z_1 = u)
  }
  pos <- declare_potential_outcomes(potential_outcomes_function = my_po_function_qd)
  return(pop)
}

design_func(numb = 5)()

g <- quick_design(design_func, numb = 5)
