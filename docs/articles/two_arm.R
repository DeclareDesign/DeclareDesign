## ----MIDA, echo = FALSE,include = FALSE----------------------------------
library(DeclareDesign)

two_arm_template <- function(N = 500, n = 250, m = 100, tau = 1, sigma = 1) {
  population <- declare_population( 
   N = N, noise = rnorm(N), treatment_effect = rnorm(N, mean = tau, sd = sigma))
  potential_outcomes <- declare_potential_outcomes(
  	Y_Z_0 = noise, 
  	Y_Z_1 = noise + treatment_effect)
	sampling <- declare_sampling(n = n)
	assignment <- declare_assignment(m = m)
	estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
	estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)
	declare_design(
		population, potential_outcomes, sampling, estimand, assignment, reveal_outcomes, estimator)
}

attr(two_arm_template,"tips") <- c(
	N = "Size of population", n = "Size of sample", 
	m = "Number assigned to treatment", tau = "Average treatment effect", 
	sigma = "Additional (square root of) variance added by treatment"
)

saveRDS(two_arm_template,"two_arm_template.RDS")


## ------------------------------------------------------------------------
# Model ------------------------------------------------------------------------
N <- 500
n <- 250
m <- 100
tau <- 1
sigma <- 3
population <- declare_population( 
 N = N, noise = rnorm(N), treatment_effect = rnorm(N, mean = tau, sd = sigma))
potential_outcomes <- declare_potential_outcomes(
	Y_Z_0 = noise, 
	Y_Z_1 = noise + treatment_effect)

# Inquiry ----------------------------------------------------------------------
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# Data Strategy ----------------------------------------------------------------
sampling <- declare_sampling(n = n)
assignment <- declare_assignment(m = m)

# Answer Strategy --------------------------------------------------------------
estimator <- declare_estimator(Y ~ Z, estimand = estimand)

# Design -----------------------------------------------------------------------
two_arm <- declare_design(population, potential_outcomes, sampling, 
													estimand, assignment, reveal_outcomes, estimator)

## ----eval = FALSE, echo = FALSE------------------------------------------
#  diagnosis <- diagnose_design(two_arm, sims = 10000, bootstrap_sims = 1000)
#  saveRDS(diagnosis,"two_arm_diagnosis.RDS")

## ----eval = FALSE, echo = TRUE-------------------------------------------
#  diagnosis <- diagnose_design(two_arm, sims = 10000, bootstrap_sims = 1000)

## ----echo = FALSE--------------------------------------------------------
diagnosis <- readRDS("two_arm_diagnosis.RDS")
diagnosands <- get_diagnosands(diagnosis)
diagnosis_table <- diagnosands[,c("mean_estimate","mean_estimand","bias","se(bias)","power","se(power)","coverage","se(coverage)")]
names(diagnosis_table) <- c("Mean Estimate", "Mean Estimand", "Bias", "SE(Bias)", "Power", "SE(Power)", "Coverage", "SE(Coverage)")
knitr::kable(diagnosis_table,digits = 3)

mean_estimate <- round(diagnosands[1,"mean_estimate"],3)
mean_estimand <- round(diagnosands[1,"mean_estimand"],3)
bias <- round(diagnosands[1,"bias"],3)
power <- round(diagnosands[1,"power"],2)*100
coverage <- round(diagnosands[1,"coverage"],2)*100

