library(DeclareDesign)

# Model a population with 500 units
population <- declare_population(N = 500,
                                 noise = rnorm(N),
                                 treatment_effect = rnorm(N, mean=1))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise,
                                                 Y_Z_1 = noise + treatment_effect)

estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# Data Strategy
sampling <- declare_sampling(n = 250) # Sample of 250 units
assignment <- declare_assignment(m = 100) # 100 treated units

# Answer Strategy
estimator <- declare_estimator(Y ~ Z, estimand = estimand)

# Design
two_arm <- declare_design(population,
                          potential_outcomes,
                          sampling,
                          estimand,
                          assignment,
                          reveal_outcomes,
                          estimator)

# Diagnose
diagnosis <- diagnose_design(two_arm,
                             sims = 1000,
                             bootstrap_sims = 500)

diagnosis
