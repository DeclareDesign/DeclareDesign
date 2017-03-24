rm(list = ls())
library(DeclareDesign)
library(magrittr)
library(dplyr)
library(DDestimate)

my_population <- declare_population(N = 500, noise = rnorm(N))

my_sampling <- declare_sampling(n = 150)

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_assignment <- declare_assignment(m = 25)
my_assignment2 <- declare_assignment(m = 50)

pate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = pate)
sate <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = sate)

pate_estimator <- declare_estimator(Y ~ Z, estimand = pate, label = pate)
sate_estimator <- declare_estimator(Y ~ Z, estimand = sate, label = sate)

#debugonce(declare_design)
god <- declare_design(my_population(),
                      my_potential_outcomes,
                      pate,
                      mutate(myvar = 5),
                      my_assignment,
                      reveal_outcomes,
                      pate_estimator)

god$data_function() %$% table(myvar)
god$design_function()



#debugonce(modify_design)

dig <- modify_design(god, mutate(myvar = 5), mutate(myvar = 10))
dig$design_function()
dig$data_function()%$% table(myvar)

#debugonce(modify_design)
dig <- modify_design(god, from_to(mutate(myvar = 5), mutate(myvar = 10)))
dig <- modify_design(god,
                     from_to(mutate(myvar = 5), mutate(myvar = 10)),
                     from_to(my_assignment, my_assignment2))

dig
dig$design_function()
god$data_function()%$% table(myvar, Z)
dig$data_function()%$% table(myvar, Z)



dig <- modify_design(god, from = pate, to = sate)



god2 <- modify_design(god)


head(god$design_function())
head(god2$design_function())



##library(doParallel)
##cl <- makeCluster(4)
##registerDoParallel(cl)

the_deal <- diagnose_design(god, sims = 100)


the_deal$simulations %>% head

the_deal$diagnosands


