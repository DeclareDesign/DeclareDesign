## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
set.seed(42)
library(DeclareDesign)
options(digits=2)

my_population <-
  declare_population(N = 1000,
  income = rnorm(N),
  age = sample(18:95, N, replace = TRUE))

pop <- my_population()

my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z)
pop_pos <- my_potential_outcomes(pop)

my_sampling <- declare_sampling(n = 250)
smp <- my_sampling(pop_pos)

my_assignment <- declare_assignment(m = 25)
smp <- my_assignment(smp)

my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

smp <- reveal_outcomes(smp)

## ----echo=TRUE, results="hide"-------------------------------------------
my_population_function <- function(N) {
  data.frame(u = rnorm(N))
}

my_population_custom <- declare_population(
  handler = my_population_function, N = 100)

pop_custom <- my_population_custom()

head(pop_custom)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_custom))

## ----echo=TRUE, results="hide"-------------------------------------------
my_potential_outcomes_function <-
  function(data) {
    data$Y_Z_0 <- with(data, u)
    data$Y_Z_1 <- with(data, 0.25 + u)
    data
  }
my_potential_outcomes_custom <- declare_potential_outcomes(
  handler = my_potential_outcomes_function
)

pop_pos_custom <- my_potential_outcomes_custom(pop_custom)

head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")])

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")]))

## ----echo=TRUE, results="hide"-------------------------------------------
my_sampling_function <- function(data) {
     data$S <- rbinom(n = nrow(data),
            size = 1,
            prob = 0.1)
     data[data$S == 1, ]
}

my_sampling_custom <- declare_sampling(
  handler = my_sampling_function)

smp_custom <- my_sampling_custom(pop_pos)

nrow(smp_custom)

## ----echo=TRUE, results="hide"-------------------------------------------
my_assignment_function <- function(data) {
  data$Z <- rbinom(n = nrow(data),
         size = 1,
         prob = 0.5)
  data
}
my_assignment_custom <- declare_assignment(
  handler = my_assignment_function)

table(my_assignment_custom(pop_pos)$Z)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(t(as.matrix(table(my_assignment_custom(pop_pos)$Z))))

## ----echo=TRUE, results="hide"-------------------------------------------
my_estimand_function <- function(data, label) {
    data.frame(
      estimand_label=label,
      estimand=with(data, median(Y_Z_1 - Y_Z_0))
    )
}
my_estimand_custom <- declare_estimand(
  handler = my_estimand_function, label = "medianTE")

my_estimand_custom(pop_pos)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_estimand_custom(pop_pos))

## ----echo=TRUE, results="hide"-------------------------------------------
my_estimator_function <- function(data){
  data.frame(est = with(data, mean(Y)))
}

my_estimator_custom <- 
  declare_estimator(handler = tidy_estimator(my_estimator_function), 
                    estimand = my_estimand)

my_estimator_custom(smp)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_estimator_custom(smp))

