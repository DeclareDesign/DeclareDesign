## ----include=FALSE-------------------------------------------------------
set.seed(42)
options(digits=2)
library(knitr)
library(DeclareDesign)

## ----echo=TRUE, eval=TRUE------------------------------------------------
# M -- Model: Speculation on variables and relations between them
population <- declare_population(N = 100, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, 
                                                 Y_Z_1 = u + 1)

# I -- Inquiry: A query defined in terms of potential outcomes
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D -- Data Strategy: Researcher interventions on the world
assignment <- declare_assignment(m = 50) 

# A -- Answer Strategy: Conclusions to be drawn from data
estimator <- declare_estimator(Y ~ Z, estimand = estimand)

# Design: Putting it all together
design <- declare_design(population, 
                         potential_outcomes, 
                         estimand, 
                         assignment, 
                         reveal_outcomes, 
                         estimator,
                         description = "A very simple design")

## ------------------------------------------------------------------------
data <- draw_data(design)

## ---- echo = FALSE-------------------------------------------------------
kable(head(data),digits = 2)

## ------------------------------------------------------------------------
estimates <- get_estimates(design)

## ---- echo = FALSE-------------------------------------------------------
kable(estimates,digits = 2)

## ---- eval = FALSE-------------------------------------------------------
#  diagnosis <- diagnose_design(design, sims = 10000, bootstrap_sims = 500)

## ---- echo = FALSE-------------------------------------------------------
# Speed site building. Run this code to achieve same results:
# diagnosis <- get_diagnosands(diagnose_design(design, sims = 10000, bootstrap_sims = 500))
temp_d <- data.frame(c(4.2e-06, 0.002), c(0.2, 0.0013),
 c(1, 0.00039), c(0.95,0.0021), c(1, 0.002), c(1,0.002), c(0.2, 0.0013), c(0,0))
# temp_d <- data.frame(c("0.00", "(0.00)"), c("0.00", "(0.00)"),
 # c("1.00", "(0.00)"), c("1.00", "(0.00)"), c("1.00", "(0.00)"), c(1, "(0.00)"), c(0.2, "(0.00)"), c("0.00", "(0.00)"))
colnames(temp_d) <- c("Bias", "RMSE", "Power", "Coverage","Mean Estimand", "Mean Estimate", "SD Estimate", "Type S-Rate")
rownames(temp_d) <- c("Diagnosand", "Boostrapped SE")

kable(temp_d, align = c("cccccccc"))

