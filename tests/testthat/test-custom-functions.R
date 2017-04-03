library(DeclareDesign)
library(magrittr)

## population
my_population_function <- function(N) {
  data.frame(u = rnorm(N))
}

my_population_custom <- declare_population(
  population_function = my_population_function, N = 100)

rm(my_population_function)
pop_custom <- my_population_custom()

head(pop_custom)

## potential outcomes
my_potential_outcomes_function <-
  function(data) {
    data$Y_Z_0 <- with(data, u)
    data$Y_Z_1 <- with(data, 0.25 + u)
    data
  }

##debugonce(declare_potential_outcomes)
my_potential_outcomes_custom <- declare_potential_outcomes(
  potential_outcomes_function = my_potential_outcomes_function
)

##debugonce(my_potential_outcomes_custom)

rm(my_potential_outcomes_function)
pop_custom <- my_potential_outcomes_custom(data = pop_custom)

head(pop_custom)



## sampling

my_sampling_function <- function(data) {
  rbinom(n = nrow(data),
         size = 1,
         prob = 0.1)
}

debugonce(declare_sampling)
my_sampling_custom <- declare_sampling(
  sampling_function = my_sampling_function)

smp_custom <- my_sampling_custom(pop_custom)

## does not work
table(my_assignment_custom(pop)$Z)

