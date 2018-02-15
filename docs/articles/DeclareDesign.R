## ---- echo = FALSE, message = FALSE, error = FALSE, warning = FALSE, output = FALSE----
library(DeclareDesign)
set.seed(42)
options(digits=2)

## ----echo=TRUE, results="hide"-------------------------------------------
my_population <-
  declare_population(N = 1000,
  income = rnorm(N),
  age = sample(18:95, N, replace = TRUE))

pop <- my_population()
head(pop)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop))

## ----echo=TRUE, results="hide"-------------------------------------------
my_population_nested <- declare_population(
  districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
  villages = add_level(N = 10, altitude = rnorm(N)),
  individuals = add_level(N = sample(100:200, size = 250, replace = TRUE), 
                      income = rnorm(N),
                      age = sample(18:95, N, replace = TRUE)))

## ----echo=TRUE, results="hide"-------------------------------------------
region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
pop_level_data <- declare_population(
  regions = add_level(N = 2, gdp = runif(N)),
  cities = add_level(N = 2, subways = rnorm(N, mean = 5)))

head(pop_level_data())

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_level_data()))

## ----echo=TRUE, results="hide"-------------------------------------------
country_data <- data.frame(
  cow_code = c(504, 15, 100, 90),
  polity_iv = c(-9, 7, -1, 3))
pop_data <- declare_population(data = country_data)

head(pop_data())

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_data()))

## ----echo=TRUE, results="hide"-------------------------------------------
pop_data_bootstrap <- declare_population(
  data = country_data, handler = fabricatr::resample_data)

head(pop_data_bootstrap())

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_data_bootstrap()))

## ----echo=TRUE, results="hide"-------------------------------------------
my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z)
pop_pos <- my_potential_outcomes(pop)
head(pop_pos)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_pos))

## ----echo=TRUE, results="hide"-------------------------------------------
my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z,
  conditions = 1:4)
head(my_potential_outcomes(pop))

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(my_potential_outcomes(pop)))

## ----echo=TRUE, results="hide"-------------------------------------------
my_potential_outcomes <-
  declare_potential_outcomes(
    Y_Z_0 = .05,
    Y_Z_1 = .30 + .01 * age)

head(my_potential_outcomes(pop))

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(my_potential_outcomes(pop)))

## ----echo=TRUE, results="hide"-------------------------------------------
my_sampling <- declare_sampling(n = 250)
smp <- my_sampling(pop_pos)
nrow(smp)

## ----echo=TRUE, results="hide"-------------------------------------------
my_assignment <- declare_assignment(m = 25)
smp <- my_assignment(smp)
table(smp$Z)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(t(as.matrix(table(smp$Z))))

## ----echo=TRUE, results="hide"-------------------------------------------
head(smp)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(smp))

## ----echo=TRUE, results="hide"-------------------------------------------
my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
my_estimand(pop_pos)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_estimand(pop_pos))

## ----echo=TRUE, results="hide"-------------------------------------------
smp <- reveal_outcomes(smp)
my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
my_estimator_dim(smp)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_estimator_dim(smp))

## ----echo=TRUE, results="hide"-------------------------------------------
my_estimator_lm <- 
  declare_estimator(Y ~ Z, 
                    model = lm_robust, 
                    coefficient_name = "Z", 
                    estimand = my_estimand)

my_estimator_lm(smp)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_estimator_lm(smp))

## ----echo=TRUE, results="hide"-------------------------------------------
design <- declare_design(my_population,
                         my_potential_outcomes,
                         my_estimand,
                         my_sampling,
                         my_assignment,
                         reveal_outcomes,
                         my_estimator_dim)

## ----echo=TRUE, results="hide"-------------------------------------------
dat <- draw_data(design)
head(dat)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(dat))

## ----echo=TRUE, results="hide"-------------------------------------------
get_estimates(design)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(get_estimates(design))

## ----echo=TRUE, results="hide"-------------------------------------------
get_estimands(design)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(get_estimands(design))

