
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

pop_custom <- my_population_custom()

# draw POs for it without arguments

my_po_function <- function(data) {
  data %>% mutate(Y_Z_0 = .25 + u, Y_Z_1 = u)
}

##debugonce(declare_potential_outcomes)
my_po_custom <- declare_potential_outcomes(
  potential_outcomes_function = my_po_function)

##debugonce(my_po_custom)
rm(my_po_function)
pop_custom <- my_po_custom(pop_custom)

head(pop_custom)

## draw POs for it with arguments

my_po_function <- function(data, q) {
  data %>% mutate(Y_Z_0 = q + u, Y_Z_1 = u)
}

##debugonce(declare_potential_outcomes)
my_po_custom <- declare_potential_outcomes(
  potential_outcomes_function = my_po_function, q = rnorm(1))

##debugonce(my_po_custom)
rm(my_po_function)
pop_custom <- my_po_custom(pop_custom)

head(pop_custom)


# do quick design

# design_func <- function(N){
#   pop <- declare_population(N = N)
#   my_po_function_qd <- function(data) {
#     data %>% mutate(Y_Z_0 = .25 + u, Y_Z_1 = u)
#   }
#   pos <- declare_potential_outcomes(potential_outcomes_function = my_po_function_qd)
#   return(pop)
# }
#
# design_func(N = 5)()

