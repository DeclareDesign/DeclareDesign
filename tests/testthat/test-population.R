
library(DDfabricate)
library(DeclareDesign)
library(magrittr)

# custom function

my_population_function <- function(N) {
  data.frame(u = rnorm(N))
}

Q <- 55

bob <- data.frame(B = runif(10000000))

#debugonce(DeclareDesign:::make_call_DD)
##debugonce(declare_population)
my_population_custom <- declare_population(
  population_function = my_population_function, N = Q)

## works
rm(my_population_function)

##debugonce(DeclareDesign:::lazy_eval_DD)

rm(list = ls()[!(ls() %in% c("my_population_custom"))])

##debugonce(my_population_custom)
pop_custom <- my_population_custom()

head(pop_custom)

## default function

my_population_default <- declare_population(N = 100, q = rnorm(5))

## works
pop_default <- my_population_default()

head(pop_default)

## do quick design

design_func <- function(numb){
  pop <- declare_population(N = numb, q = rnorm(5))
  rm(numb)
  return(pop)
}

rm(list = ls()[!(ls() %in% "design_func")])
design_func(numb = 5)()

my_design <- design_func(numb = 5)


# will work when DDfabricate is fixed
# declare_population(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))()
#
# declare_population(districts = level(N = 5, gdp = rnorm(N)),
#                    villages = level(N = 12, subways = rnorm(N, mean = gdp)))()

