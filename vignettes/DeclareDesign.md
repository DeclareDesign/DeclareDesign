---
title: "Diagnosing research designs with DeclareDesign"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Diagnosing research designs with DeclareDesign}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




# Overview

The **DeclareDesign** package has a small set of core functions. Six core functions are used to describe key steps in a research design:

1. `declare_population()` (describes dimensions and distributions over the variables in the population)
2. `declare_potential_outcomes()` (takes population or sample and adds potential outcomes produced by interventions)
3. `declare_sampling()` (takes a population and selects a sample)
4. `declare_assignment()` (takes a population or sample and adds treatment assignments)
5. `declare_estimand()` (takes potential outcomes and calculates a quantity of interest)
6. `declare_estimator()` (takes data produced by sampling and assignment and returns estimates)

Each of the `declare_*()` functions returns a *function*.  The function `declare_design()` can take any of these six functions, plus any R function that takes data and returns data.

Once you have declared your design, there are six core post-design-declaration commands used to modify or diagnose your design:

1. `modify_design()` (takes a design and a set of modifications, returns a design)
2. `diagnose_design()` (takes a design, returns simulations and diagnosis)
3. `compare_designs()` (takes a list of designs and diagnoses them all)
4. `draw_data()` (takes a design and returns a single draw of the data)
5. `get_estimates()` (takes a design a returns a single simulation of estimates)
6. `get_estimands()` (takes a design a returns a single simulation of estimands)

There are a few other features:

1. A template is a function that takes parameters (e.g., `N`) and returns a design. `expand_design()` is a function of a template and parameters that returns a design.
2. We can easily `declare_diagnosands()`, which are things like power and bias, but the package defaults to the usual suspects.
3. `reveal_outcomes()` implements a general switching equation, which allows you to reveal outcomes from potential outcomes and a treatment assignment.

# Design Declaration

Design declaration is achieved by possibly repeated application of some or all of the declaration functions. We describe the six core functions in turn.

## Population

The function `declare_population()` can be used to define a single-level or multilevel population. The output is another function that produces a dataframe with possibly multiple background variables drawn.

When creating a single level population the only privileged name is `N`, which is used to denote the size of the population. After this, declaration proceeds using existing R functions plus some tailored functions developed as part of **DeclareDesign**.

Here is an illustration of `declare_population()` being used to create a single level dataset:


```r
my_population <-
  declare_population(N = 1000,
  income = rnorm(N),
  age = sample(18:95, N, replace = TRUE))

pop <- my_population()
head(pop)
```

|ID   | income| age|
|:----|------:|---:|
|0001 |   1.37|  95|
|0002 |  -0.56|  52|
|0003 |   0.36|  72|
|0004 |   0.63|  87|
|0005 |   0.40|  83|
|0006 |  -0.11|  75|

Multi-level datasets are also easy to produce. For multilevel data you set the `N` of each level in a call to [`level()`](/R/fabricatr/reference/level.html). The [`add_level()`](/R/fabricatr/reference/level.html) function is intelligent -- if the previous level has data, it merges so that there are N entries for each of the units at the higher level. `declare_population()`can handle non-fixed number of units at each level too.  In the example below, we have  drawn a random number of individuals that are in each village (see the `individuals` line).


```r
my_population_nested <- declare_population(
  districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
  villages = add_level(N = 10, altitude = rnorm(N)),
  individuals = add_level(N = sample(100:200, size = 250, replace = TRUE),
                      income = rnorm(N),
                      age = sample(18:95, N, replace = TRUE)))
```

In this example there are 25 districts, 10 villages per districts, and then between 100 and 200 individuals per village. The function creates districts first, then merges in villages, then merges in individuals using ID variables created at the level above it.

Within those levels, you can add in existing data (and also add new variables to them if you wish):


```r
region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
pop_level_data <- declare_population(
  regions = add_level(N = 2, gdp = runif(N)),
  cities = add_level(N = 2, subways = rnorm(N, mean = 5)))

head(pop_level_data())
```

|regions |  gdp|cities | subways|
|:-------|----:|:------|-------:|
|1       | 0.63|1      |     5.9|
|1       | 0.63|2      |     3.8|
|2       | 0.76|3      |     6.0|
|2       | 0.76|4      |     2.9|


Similarly, you can easily declare your existing data as the population:


```r
country_data <- data.frame(
  cow_code = c(504, 15, 100, 90),
  polity_iv = c(-9, 7, -1, 3))
pop_data <- declare_population(data = country_data)

head(pop_data())
```

| cow_code| polity_iv|
|--------:|---------:|
|      504|        -9|
|       15|         7|
|      100|        -1|
|       90|         3|


If you don't want your data to be fixed, you can resample from it, i.e.


```r
pop_data_bootstrap <- declare_population(
  data = country_data, handler = fabricatr::resample_data)

head(pop_data_bootstrap())
```

| cow_code| polity_iv|
|--------:|---------:|
|      100|        -1|
|      504|        -9|
|       90|         3|
|       90|         3|

Note that **fabricatr** is one of the helper packages that come along with **DeclareDesign**. **fabricatr** helps you simulate population data or resample from existing data.

## Potential outcomes

A `declare_potential_outcomes()` declaration also returns a function. That function takes data and returns data with potential outcomes columns appended. There are two ways of declaring potential outcomes, either as a formula or as separate variables (as in `declare_population()`).

### In a formula


```r
my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z)
pop_pos <- my_potential_outcomes(pop)
head(pop_pos)
```

|ID   | income| age| Y_Z_0| Y_Z_1|
|:----|------:|---:|-----:|-----:|
|0001 |   1.37|  95|     0|  1.20|
|0002 |  -0.56|  52|     0|  0.77|
|0003 |   0.36|  72|     0|  0.97|
|0004 |   0.63|  87|     0|  1.12|
|0005 |   0.40|  83|     0|  1.08|
|0006 |  -0.11|  75|     0|  1.00|


This has defaults set for conditions (0, 1) and the assignment variable name (Z). You can set the "domain" of the potential outcomes function with `conditions`.


```r
my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z,
  conditions = 1:4)
head(my_potential_outcomes(pop))
```

|ID   | income| age| Y_Z_1| Y_Z_2| Y_Z_3| Y_Z_4|
|:----|------:|---:|-----:|-----:|-----:|-----:|
|0001 |   1.37|  95|  1.20|   2.4|   3.6|   4.8|
|0002 |  -0.56|  52|  0.77|   1.5|   2.3|   3.1|
|0003 |   0.36|  72|  0.97|   1.9|   2.9|   3.9|
|0004 |   0.63|  87|  1.12|   2.2|   3.4|   4.5|
|0005 |   0.40|  83|  1.08|   2.2|   3.2|   4.3|
|0006 |  -0.11|  75|  1.00|   2.0|   3.0|   4.0|

### As separate variables

The second way is to define each potential outcome yourself. This bakes in the condition names and assignment variable.


```r
my_potential_outcomes <-
  declare_potential_outcomes(
    Y_Z_0 = .05,
    Y_Z_1 = .30 + .01 * age)

head(my_potential_outcomes(pop))
```

|ID   | income| age| Y_Z_0| Y_Z_1|
|:----|------:|---:|-----:|-----:|
|0001 |   1.37|  95|  0.05|  1.25|
|0002 |  -0.56|  52|  0.05|  0.82|
|0003 |   0.36|  72|  0.05|  1.02|
|0004 |   0.63|  87|  0.05|  1.17|
|0005 |   0.40|  83|  0.05|  1.13|
|0006 |  -0.11|  75|  0.05|  1.05|

## Sampling

A sampling function takes data and returns a sampled subset of the data. By default, `declare_sampling()` understands arguments passed to `...` as **randomizr** arguments, but it's easy to supply your own function instead.

Here we declare simple random sampling of 250 units.


```r
my_sampling <- declare_sampling(n = 250)
smp <- my_sampling(pop_pos)
nrow(smp)
```
250

In more general applications `declare_sampling()` can allow for more complex stratified and  clustered sampling.

## Assignment

Assignment declarations return functions of data that return data. If you use the **randomizr** defaults, then it appends to the dataset an assignment draw and a vector of observed probability weights.

Here we declare simple random assignment of 25 units to a binary treatment.


```r
my_assignment <- declare_assignment(m = 25)
smp <- my_assignment(smp)
table(smp$Z)
```

|   0|  1|
|---:|--:|
| 225| 25|



```r
head(smp)
```

|ID   | income| age| Y_Z_0| Y_Z_1| S_inclusion_prob|  Z| Z_cond_prob|
|:----|------:|---:|-----:|-----:|----------------:|--:|-----------:|
|0005 |   0.40|  83|     0|  1.08|             0.25|  0|         0.9|
|0008 |  -0.09|  83|     0|  1.08|             0.25|  0|         0.9|
|0009 |   2.02|  30|     0|  0.55|             0.25|  0|         0.9|
|0015 |  -0.13|  18|     0|  0.43|             0.25|  0|         0.9|
|0018 |  -2.66|  70|     0|  0.95|             0.25|  1|         0.1|
|0021 |  -0.31|  47|     0|  0.72|             0.25|  1|         0.1|


## Estimands

Estimands run on data that includes potential outcomes.

In this example, as in many applications, the estimand is defined as a function of potential outcomes: the average difference between outcomes for each unit when they are in the treatment condition and when they are in the control condition. The information needed to define the estimand is available thanks to the  `declare_potential_outcomes()` call even though it will in general not be available in datasets used by researchers conducting estimation.


```r
my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
my_estimand(pop_pos)
```

|estimand_label | estimand|
|:--------------|--------:|
|ATE            |     0.82|

The only part baked in to **DeclareDesign** is the naming structure, *outcome_assignment_condition*. You could write your own potential outcomes function to avoid this (note though that in most cases this would also require writing your own `reveal_outcomes()` function).

## Estimators

To declare an estimator, you declare an estimator function, a [`difference_in_means()`](/R/estimatr/reference/difference_in_means.html) by default. Optionally you also declare an estimand that is attached to the estimator.


```r
smp <- reveal_outcomes(smp)
my_estimator_dim <- declare_estimator(Y ~ Z, estimand = my_estimand)
my_estimator_dim(smp)
```

```
## Called from: (FUN)(data = data, ~(Y ~ Z), estimand = ~my_estimand, label = ~"my_estimator")
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#200: ret <- data.frame(estimator_label = label, ret, stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#206: estimand_label <- get_estimand_label(estimand)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#207: if (length(estimand_label) > 0) {
##     ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##         stringsAsFactors = FALSE)
## }
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#208: ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##     stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#210: ret
```



|estimator_label |coefficient |  est|   se|  p| ci_lower| ci_upper|estimand_label |
|:---------------|:-----------|----:|----:|--:|--------:|--------:|:--------------|
|my_estimator    |Z           | 0.86| 0.04|  0|     0.77|     0.94|ATE            |


The below example uses our function `lm_robust()`, which is a simple, faster drop-in replacement for `R`'s built-in `lm()` that features robust standard errors (HC2 by default, or your choice of other heteroskedasticity-consistent standard errors):


```r
my_estimator_lm <- 
  declare_estimator(Y ~ Z, 
                    model = lm_robust, 
                    coefficients = "Z", 
                    estimand = my_estimand)

my_estimator_lm(smp)
```

```
## Called from: (FUN)(data = data, ~(Y ~ Z), model = ~lm_robust, coefficients = ~"Z", 
##     estimand = ~my_estimand, label = ~"my_estimator")
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#200: ret <- data.frame(estimator_label = label, ret, stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#206: estimand_label <- get_estimand_label(estimand)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#207: if (length(estimand_label) > 0) {
##     ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##         stringsAsFactors = FALSE)
## }
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#208: ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##     stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#210: ret
```



|estimator_label |coefficient |  est|   se|  p| ci_lower| ci_upper|estimand_label |
|:---------------|:-----------|----:|----:|--:|--------:|--------:|:--------------|
|my_estimator    |Z           | 0.86| 0.04|  0|     0.77|     0.94|ATE            |


# Declaring Designs

Instead of defining your population, potential outcomes, and so on, you simply give us an ordered set of functions, e.g. beginning with a population, then potential outcomes, sampling, and so on. You can also put any `R` function in causal order that takes data and returns data -- including all the nice functions in `dplyr` like `mutate()`, to allow you to create new variables and do things like collapse clusters.

Here is an example of a design declaration:


```r
design <- declare_design(my_population,
                         my_potential_outcomes,
                         my_estimand,
                         my_sampling,
                         my_assignment,
                         reveal_outcomes,
                         my_estimator_dim)
```

Remarks re: `declare_design()`:

1. The first argument must always be a dataset or create one.
2. Your estimand is placed where you want to define it, i.e. here we are defining a PATE by placing the estimand just after population and before sampling or assignment.
3. `declare_design()` produces two things: a "dgp function" and a "design function."  The dgp function draws a dataset and the design function returns an estimands dataframe and an estimates data frame. It simulates the design from population through estimates, in whatever order you tell it -- meaning it carefully separates the data generating parts of the design and the calculation of estimates and estimands.

You can run them directly via:


```r
dat <- draw_data(design)
head(dat)
```

|ID   | income| age| Y_Z_0| Y_Z_1| S_inclusion_prob|  Z| Z_cond_prob|    Y|
|:----|------:|---:|-----:|-----:|----------------:|--:|-----------:|----:|
|0002 |  -2.49|  57|  0.05|  0.87|             0.25|  0|         0.9| 0.05|
|0004 |  -1.42|  59|  0.05|  0.89|             0.25|  0|         0.9| 0.05|
|0011 |   0.64|  61|  0.05|  0.91|             0.25|  0|         0.9| 0.05|
|0018 |   0.49|  65|  0.05|  0.95|             0.25|  0|         0.9| 0.05|
|0020 |   0.95|  46|  0.05|  0.76|             0.25|  0|         0.9| 0.05|
|0025 |   0.01|  33|  0.05|  0.63|             0.25|  0|         0.9| 0.05|

and


```r
get_estimates(design)
```

```
## Called from: (FUN)(data = data, ~(Y ~ Z), estimand = ~my_estimand, label = ~"my_estimator")
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#200: ret <- data.frame(estimator_label = label, ret, stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#206: estimand_label <- get_estimand_label(estimand)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#207: if (length(estimand_label) > 0) {
##     ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##         stringsAsFactors = FALSE)
## }
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#208: ret <- cbind(ret, estimand_label = estimand_label, row.names = NULL, 
##     stringsAsFactors = FALSE)
## debug at /Users/gblair/research/declaredesign/R/declare_estimator.R#210: ret
```



|estimator_label |coefficient |  est|   se|  p| ci_lower| ci_upper|estimand_label |
|:---------------|:-----------|----:|----:|--:|--------:|--------:|:--------------|
|my_estimator    |Z           | 0.81| 0.05|  0|     0.72|     0.91|ATE            |


```r
get_estimands(design)
```

|estimand_label | estimand|
|:--------------|--------:|
|ATE            |     0.81|

# Next steps

Next, we recommend you read the [Custom functions](/R/DeclareDesign/articles/custom_functions.html) tutorial to learn how to provide custom functions to replace any step of `declare_design()`. You can also read about [`expand_design()` and other advanced features](/R/DeclareDesign/articles/advanced_features.html) in our Advanced features tutorial.

Finally, you can [learn about DeclareDesign’s companion packages](/R/DeclareDesign/articles/companion_packages.html).
