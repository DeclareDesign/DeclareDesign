
library(DDfabricate)
library(DeclareDesign)
library(magrittr)



# works
declare_population(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))()

declare_population(districts = level(N = 5, gdp = rnorm(N)),
                   villages = level(N = 12, subways = rnorm(N, mean = gdp)))()

