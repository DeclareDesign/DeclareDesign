---
title: "Advanced features"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Advanced features}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Quick design

You can also write a design maker function that declares a design based on a set of parameters like `N`, the number of clusters, etc. and use the function `expand_design()` to make designs using just those parameters.


```r
m_arm_trial <- function(numb){
  my_population <- declare_population(
    N = numb, income = rnorm(N), age = sample(18:95, N, replace = T))

  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z)
  my_sampling <- declare_sampling(n = 250)
  my_assignment <- declare_assignment(m = 25)
  my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
  my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
  my_reveal <- declare_reveal()
  my_design <- declare_design(my_population,
                              my_potential_outcomes,
                              my_estimand,
                              my_sampling,
                              my_assignment,
                              my_reveal,
                              my_estimator_dim)
  return(my_design)
}

my_1000_design <- expand_design(template = m_arm_trial, numb = 1000)
head(draw_data(my_1000_design))
```

|ID   | income| age| Y_Z_0| Y_Z_1| S_inclusion_prob|  Z| Z_cond_prob|  Y|
|:----|------:|---:|-----:|-----:|----------------:|--:|-----------:|--:|
|0007 |  -0.96|  51|     0|  0.76|             0.25|  0|         0.9|  0|
|0010 |   1.51|  40|     0|  0.65|             0.25|  0|         0.9|  0|
|0011 |   2.35|  75|     0|  1.00|             0.25|  0|         0.9|  0|
|0020 |  -0.28|  21|     0|  0.46|             0.25|  0|         0.9|  0|
|0023 |  -0.02|  55|     0|  0.80|             0.25|  0|         0.9|  0|
|0027 |   0.27|  51|     0|  0.76|             0.25|  0|         0.9|  0|

# Continuous potential outcomes


```r
my_potential_outcomes_continuous <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z, conditions = seq(0, 1, by = .1))

continuous_treatment_function <- function(data){
 data$Z <- sample(seq(0, 1, by = .1), size = nrow(data), replace = TRUE)
 data
}

my_assignment_continuous <- declare_assignment(handler = continuous_treatment_function)

my_reveal <- declare_reveal()


my_design <- declare_design(my_population(),
                            my_potential_outcomes_continuous,
                            my_assignment_continuous,
                            my_reveal)

head(draw_data(my_design))
```

|ID   | income| age| Y_Z_0| Y_Z_0.1| Y_Z_0.2| Y_Z_0.3| Y_Z_0.4| Y_Z_0.5| Y_Z_0.6| Y_Z_0.7| Y_Z_0.8| Y_Z_0.9| Y_Z_1|   Z|    Y|
|:----|------:|---:|-----:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-----:|---:|----:|
|0001 |   0.30|  35|     0|    0.06|    0.12|    0.18|    0.24|    0.30|    0.36|    0.42|    0.48|    0.54|  0.60| 0.8| 0.48|
|0002 |   0.37|  73|     0|    0.10|    0.20|    0.29|    0.39|    0.49|    0.59|    0.69|    0.78|    0.88|  0.98| 0.3| 0.29|
|0003 |   1.85|  36|     0|    0.06|    0.12|    0.18|    0.24|    0.30|    0.37|    0.43|    0.49|    0.55|  0.61| 0.3| 0.18|
|0004 |  -0.50|  26|     0|    0.05|    0.10|    0.15|    0.20|    0.26|    0.31|    0.36|    0.41|    0.46|  0.51| 0.0| 0.00|
|0005 |   0.06|  19|     0|    0.04|    0.09|    0.13|    0.18|    0.22|    0.26|    0.31|    0.35|    0.40|  0.44| 0.5| 0.22|
|0006 |  -0.06|  50|     0|    0.08|    0.15|    0.23|    0.30|    0.38|    0.45|    0.52|    0.60|    0.68|  0.75| 0.5| 0.38|

# Attrition

Attrition can be thought of as just another potential outcome. That is, one describe possible attrition processes, include them in the design, and see how estimation strategies are affected by these processes. Here is an example. 


```r
my_potential_outcomes_attrition <- declare_potential_outcomes(
  formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))

reveal_R <- declare_reveal(outcome_variables = "R")
reveal_Y <- declare_reveal(outcome_variables = "Y", attrition_variables = "R")

my_design <- declare_design(my_population(),
                            my_potential_outcomes,
                            my_potential_outcomes_attrition,
                            my_assignment,
                            reveal_R,
                            reveal_Y)

head(draw_data(my_design)[, c("ID", "Y_Z_0", "Y_Z_1", "R_Z_0", "R_Z_1", "Z", "R", "Y")])
```

|ID   | Y_Z_0| Y_Z_1| R_Z_0| R_Z_1|  Z|  R|  Y|
|:----|-----:|-----:|-----:|-----:|--:|--:|--:|
|0001 |     0|  0.85|     1|     0|  0|  1|  0|
|0002 |     0|  1.13|     1|     1|  0|  1|  0|
|0003 |     0|  1.04|     0|     1|  0|  0| NA|
|0004 |     0|  0.90|     1|     1|  0|  1|  0|
|0005 |     0|  1.14|     1|     1|  0|  1|  0|
|0006 |     0|  0.73|     0|     0|  0|  0| NA|

# Stochastic population sizes

The population (or any level of the population) can have stochastic population sizes. (In fact, N can be a number, a fixed vector of numbers, or an expression that returns a stochastic number or vector of numbers.)


```r
stochastic_population <- declare_population(
  N = sample(500:1000, 1), income = rnorm(N), age = sample(18:95, N, replace = TRUE))

c(nrow(stochastic_population()), 
  nrow(stochastic_population()), 
  nrow(stochastic_population()))
```
768, 815, 950
