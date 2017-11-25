
<!-- README.md is generated from README.Rmd. Please edit that file -->
The Key Idea
------------

The key idea motivating `DeclareDesign` is that the core analytic features of research designs can be declared in a complete manner and saved as an object. A properly declared design can then easily be shared, modified, improved, and used. The design contains the information needed to implement key parts of data generation and to implement analysis. It also contains enough information to allow researchers or third parties to query it to see whether it can deliver on its promises.

### 1. A simple design declaration

Here is an illustration using a very simple two arm trial.

``` r
# M -- Model: Speculation on variables and relations between them
population         <- declare_population(N = 100, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, 
                                                 Y_Z_1 = 1 - u)

# I -- Inquiry: A query defined in terms of potential outcomes
estimand           <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D -- Data Strategy: Researcher interventions on the world
assignment         <- declare_assignment(m = 50) 

# A -- Answer Strategy: Conclusiosn to be drawn from data
estimator          <- declare_estimator(Y ~ Z, estimand = estimand)

# Design: Putting it all together
my_design            <- declare_design(population, 
                          potential_outcomes, 
                          estimand, 
                          assignment, 
                          reveal_outcomes, 
                          estimator,
                          description = "A very simple design")
```

### 2. Making Use of A Design

Use the design object to simulate data, including treatment assignments:

``` r
my_data <- draw_data(my_design)
```

| ID  |      u|  Y\_Z\_0|  Y\_Z\_1|    Z|  Z\_cond\_prob|      Y|
|:----|------:|--------:|--------:|----:|--------------:|------:|
| 001 |   1.37|     1.37|    -0.37|    1|            0.5|  -0.37|
| 002 |  -0.56|    -0.56|     1.56|    1|            0.5|   1.56|
| 003 |   0.36|     0.36|     0.64|    1|            0.5|   0.64|
| 004 |   0.63|     0.63|     0.37|    0|            0.5|   0.63|
| 005 |   0.40|     0.40|     0.60|    0|            0.5|   0.40|
| 006 |  -0.11|    -0.11|     1.11|    1|            0.5|   1.11|

Use the design object to implement analysis:

``` r
estimates <- get_estimates(my_design)
```

| estimator\_label | coefficient\_name |  est|    se|    p|  ci\_lower|  ci\_upper| estimand\_label |
|:-----------------|:------------------|----:|-----:|----:|----------:|----------:|:----------------|
| my\_estimator    | Z                 |    1|  0.19|    0|       0.64|        1.4| ATE             |

### 3. Diagnosing a design

The fully declared design contains the information needed to diagnose it.

``` r
diagnosis <- diagnose_design(my_design, sims = 1000, bootstrap_sims = 500)
```

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

    ## Warning in serialize(data, node$con): 'package:randomizr' may not be
    ## available when loading

The diagnosis here confirms the fact that random assignment to treatment allows for unbiased estimates of treatment effects. More surprisingly perhaps, the diagnosis reveals that the statistical inferences are wrong: the 95% confidence intervals do not have 95% coverage, reflecting the fact that standard errors are overly conservative.

This project is generously supported by a grant from the [Laura and John Arnold Foundation](http://www.arnoldfoundation.org) and seed funding from [EGAP](http://egap.org).
