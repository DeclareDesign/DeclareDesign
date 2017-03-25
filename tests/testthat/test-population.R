
library(DDfabricate)
library(DeclareDesign)
library(magrittr)


declare_population(level(N = 5, gdp = rnorm(N), level_name = "regions"),
                   level(N = 12, subways = rnorm(N, mean = gdp), level_name = "cities"))()


declare_population(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))()


declare_population(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = 12, subways = rnorm(N, mean = gdp), level_name = "cities"))()

declare_population(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = 1:5, subways = rnorm(N, mean = gdp), level_name = "cities"))()

declare_population(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = sample(1:5), subways = rnorm(N, mean = gdp), level_name = "cities"))()

